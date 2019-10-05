package dali
package macros

import scala.reflect.macros.whitebox

class GenericMacros(private[macros] val c: whitebox.Context) {
  import c.universe._

  private[this] def abort(message: String): Nothing =
    c.abort(c.enclosingPosition, message)

  private[this] def type2classSymbol(t: Type): ClassSymbol = {
    val symbol = t.typeSymbol
    if (!symbol.isClass) {
      abort(s"$symbol is not a class or trait")
    }

    val classSymbol = symbol.asClass
    classSymbol.typeSignature
    classSymbol
  }

  private[this] def symbol2type(symbol: Symbol): Type = {
    if (!symbol.isType) {
      abort(s"$symbol is not type")
    }

    symbol.typeSignature
    symbol.asType.toType
  }

  private[this] val hlistType: Type = typeOf[HList]
  private[this] val coproductType: Type = typeOf[Coproduct]

  private[this] def isReprType(t: Type): Boolean =
    t <:< hlistType || t <:< coproductType

  private[this] def isProduct(t: Type): Boolean =
    t.typeSymbol.isClass && (type2classSymbol(t).isCaseClass || type2classSymbol(t).isModuleClass)

  private[this] def isCoproduct(t: Type): Boolean =
    t.typeSymbol.isClass && type2classSymbol(t).isSealed &&
      type2classSymbol(t).knownDirectSubclasses.forall(isCoproductChild(t, _))

  private[this] def isCoproductChild(t: Type, symbol: Symbol): Boolean = {
    val parentTypeCons = t.typeConstructor

    val childTypeCons = symbol2type(symbol).typeConstructor
    if (childTypeCons.typeParams.size != parentTypeCons.typeParams.size) {
      return false
    }

    val parentSymbol = parentTypeCons.typeSymbol
    val childSymbol = childTypeCons.typeSymbol

    val appliedParams = c.internal.thisType(childSymbol).baseType(parentSymbol).typeArgs.map(_.typeSymbol)
    appliedParams.zip(childTypeCons.typeParams).forall { case (x, y) => x == y }
  }

  private[this] def isHigherKind(typeCons: Type): Boolean =
    typeCons.typeParams.size == 1

  private[this] def fieldsOf(t: Type): List[(TermName, Type)] =
    t.decls.sorted.collect {
      case symbol: TermSymbol if symbol.isCaseAccessor && symbol.isGetter =>
        (symbol.name.toTermName, symbol.typeSignatureIn(t).finalResultType)
    }

  private[this] def childrenOf(t: Type): List[Type] =
    t.typeSymbol.asClass.knownDirectSubclasses.toList
      .sortBy(_.fullName)
      .map(child => appliedType(child.asType.toType, t.typeArgs))

  private[this] def hasParamType(t: Type, paramType: Type): Boolean =
    t =:= paramType || t.typeArgs.exists(hasParamType(_, paramType))

  private[this] def replaceParamType(t: Type, paramType: Type, to: TypeName): Tree =
    if (t =:= paramType) tq"$to"
    else if (t.typeArgs.isEmpty) tq"${t.typeSymbol}"
    else tq"${t.typeSymbol}[..${t.typeArgs.map(replaceParamType(_, paramType, to))}]"

  private[this] def productRepr[A](names: List[A])(cgen: A => Tree, pgen: A => Tree): (Tree, Tree) = {
    val cons = names.foldRight(q"_root_.dali.HNil": Tree) {
      case (name, acc) => q"_root_.dali.:*:(${cgen(name)}, $acc)"
    }
    val pattern = names.foldRight(pq"_root_.dali.HNil": Tree) {
      case (name, acc) => pq"_root_.dali.:*:(${pgen(name)}, $acc)"
    }

    (cons, pattern)
  }

  private[this] def productReprType(types: List[Tree]): Tree =
    types.foldRight(tq"_root_.dali.HNil": Tree) {
      case (t, acc) => tq"_root_.dali.:*:[$t, $acc]"
    }

  def materialize[A: WeakTypeTag, R]: Tree = {
    val t = weakTypeOf[A]
    if (isReprType(t)) {
      abort("no Generic instance available for HList or Coproduct")
    }

    if (isProduct(t)) materializeProduct(t)
    else if (isCoproduct(t)) materializeCoproduct(t)
    else abort(s"no Generic instance is available for $t")
  }

  private[this] def materializeProduct(t: Type): Tree = {
    if (type2classSymbol(t).isModuleClass) {
      return materializeSingleton(t)
    }

    val symbol = t.typeSymbol
    val companion = symbol.companion

    val fields = fieldsOf(t)
    val names = fields.map(_ => TermName(c.freshName("pattern$")))

    val cons = q"$companion(..$names)"
    val pattern = pq"$companion(..${names.map(name => pq"$name")})"

    val (reprCons, reprPattern) = productRepr(names)(name => q"$name", name => pq"$name")
    val reprType = productReprType(fields.map { case (_, t) => tq"$t" })

    val argName = TermName(c.freshName("arg$"))
    val className = TypeName(c.freshName("Product$"))
    q"""
      final class $className extends _root_.dali.Generic[$t] {
        type Repr = $reprType
        def embed($argName: $t): Repr = $argName match { case $pattern => $reprCons }
        def project($argName: Repr): $t = $argName match { case $reprPattern => $cons }
      }
      new $className: _root_.dali.Generic.Aux[$t, $reprType]
    """
  }

  private[this] def materializeSingleton(t: Type): Tree = {
    val singleton = t match {
      case SingleType(_, symbol) => symbol
      case _                     => abort(s"BUG: $t is not singleton")
    }

    val argName = TermName(c.freshName("arg$"))
    val className = TypeName(c.freshName("Singleton$"))
    q"""
      final class $className extends _root_.dali.Generic[$t] {
        type Repr = _root_.dali.HNil
        def embed($argName: $t): Repr = _root_.dali.HNil
        def project($argName: Repr): $t = $singleton
      }
      new $className: _root_.dali.Generic.Aux[$t, _root_.dali.HNil]
    """
  }

  private[this] def materializeCoproduct(t: Type): Tree = {
    val children = childrenOf(t)

    val reprType = children.foldRight(tq"_root_.dali.CNil": Tree) {
      case (t, acc) => tq"_root_.dali.:+:[$t, $acc]"
    }
    val cases = children.zipWithIndex.map { case (t, i) => cq"_: $t => $i" }

    val argName = TermName(c.freshName("arg$"))
    val className = TypeName(c.freshName("Coproduct$"))
    q"""
      final class $className extends _root_.dali.Generic[$t] {
        type Repr = $reprType
        def embed($argName: $t): Repr =
          _root_.dali.Coproduct.unsafeApply($argName match { case ..$cases }, $argName).asInstanceOf[Repr]
        def project($argName: Repr): $t =
          _root_.dali.Coproduct.unsafeGet($argName).asInstanceOf[$t]
      }
      new $className: _root_.dali.Generic.Aux[$t, $reprType]
    """
  }

  def materializeLabelled[A: WeakTypeTag, L <: String with Singleton, R]: Tree = {
    val t = weakTypeOf[A]
    if (isReprType(t)) {
      abort("no LabelledGeneric instance available for HList or Coproduct")
    }

    if (isProduct(t)) materializeLabelledProduct(t)
    else if (isCoproduct(t)) materializeLabelledCoproduct(t)
    else abort(s"no LabelledGeneric instance is available for $t")
  }

  private[this] def materializeLabelledProduct(t: Type): Tree = {
    if (type2classSymbol(t).isModuleClass) {
      return materializeLabelledSingleton(t)
    }

    val symbol = t.typeSymbol
    val companion = symbol.companion

    val fields = fieldsOf(t)
    val elems = fields.map {
      case (name, t) =>
        val nameType = c.internal.constantType(Constant(name.toString))
        (nameType, TermName(c.freshName("pattern$")), t)
    }

    val cons = q"$companion(..${elems.map(_._2)})"
    val pattern = pq"$companion(..${elems.map { case (_, name, _) => pq"$name" }})"

    val (reprCons, reprPattern) = productRepr(elems)(
      { case (labelType, name, _) => q"_root_.dali.Labelled[$labelType]($name)" },
      { case (_, name, _)         => pq"_root_.dali.Labelled($name)" }
    )

    val reprType = productReprType(
      elems.map { case (labelType, _, t) => tq"_root_.dali.Labelled[$labelType, $t]" }
    )

    val labelType = c.internal.constantType(Constant(t.typeSymbol.name.toString))
    val argName = TermName(c.freshName("arg$"))
    val className = TypeName(c.freshName("LabelledProduct$"))
    return q"""
      final class $className extends _root_.dali.LabelledGeneric[$t] {
        type Repr = $reprType
        def embed($argName: $t): Repr = $argName match { case $pattern => $reprCons }
        def project($argName: Repr): $t = $argName match { case $reprPattern => $cons }
        type Label = $labelType
      }
      new $className: _root_.dali.LabelledGeneric.Aux[$t, $labelType, $reprType]
    """
  }

  private[this] def materializeLabelledSingleton(t: Type): Tree = {
    val singleton = t match {
      case SingleType(_, symbol) => symbol
      case _                     => abort(s"BUG: $t is not singleton")
    }

    val labelType = c.internal.constantType(Constant(t.typeSymbol.name.toString))
    val argName = TermName(c.freshName("arg$"))
    val className = TypeName(c.freshName("LabelledSingleton$"))
    q"""
      final class $className extends _root_.dali.LabelledGeneric[$t] {
        type Repr = _root_.dali.HNil
        def embed($argName: $t): Repr = _root_.dali.HNil
        def project($argName: Repr): $t = $singleton
        type Label = $labelType
      }
      new $className: _root_.dali.LabelledGeneric.Aux[$t, $labelType, _root_.dali.HNil]
    """
  }

  private[this] def materializeLabelledCoproduct(t: Type): Tree = {
    val children = childrenOf(t)

    val reprType = children.foldRight(tq"_root_.dali.CNil": Tree) {
      case (t, acc) =>
        val labelType = c.internal.constantType(Constant(t.typeSymbol.name.toString))
        tq"_root_.dali.:+:[_root_.dali.Labelled[$labelType, $t], $acc]"
    }
    val cases = children.zipWithIndex.map { case (t, i) => cq"_: $t => $i" }

    val labelType = c.internal.constantType(Constant(t.typeSymbol.name.toString))
    val argName = TermName(c.freshName("arg$"))
    val className = TypeName(c.freshName("LabelledCoproduct$"))
    q"""
      final class $className extends _root_.dali.LabelledGeneric[$t] {
        type Repr = $reprType
        def embed($argName: $t): Repr =
          _root_.dali.Coproduct.unsafeApply($argName match { case ..$cases }, Labelled[String with Singleton]($argName)).asInstanceOf[Repr]
        def project($argName: Repr): $t =
          _root_.dali.Coproduct.unsafeGet($argName)
            .asInstanceOf[_root_.dali.Labelled[_root_.java.lang.String with _root_.scala.Singleton, $t]]
        type Label = $labelType
      }
      new $className: _root_.dali.LabelledGeneric.Aux[$t, $labelType, $reprType]
    """
  }

  def materialize1[F[_], R <: higher.TypeFunction1](implicit F: WeakTypeTag[F[_]]): Tree = {
    val typeCons = weakTypeOf[F[_]].typeConstructor
    if (!isHigherKind(typeCons)) {
      abort(s"no Generic1 instance is available for not * -> * kind type")
    }

    val param = typeCons.typeParams.head
    val paramType = param.asType.toType
    val typeA = appliedType(typeCons, paramType).dealias
    val typeCons0 = typeA.typeConstructor

    if (isProduct(typeCons0)) materializeProduct1(typeCons, paramType, typeA)
    else if (isCoproduct(typeCons0)) materializeCoproduct1(typeCons, paramType, typeA)
    else abort(s"no Generic1 instance is available for $typeCons")
  }

  private[this] def materializeProduct1(typeCons: Type, paramType: Type, typeA: Type): Tree = {
    val companion = typeA.typeSymbol.companion

    val fields = fieldsOf(typeA)
    val names = fields.map { case (name, t) => TermName(c.freshName("pattern$")) }

    val cons = q"$companion(..$names)"
    val pattern = pq"$companion(..${names.map(name => pq"$name")})"

    val (reprCons, reprPattern) = productRepr(names)(name => q"$name", name => pq"$name")

    val reprTypes = fields.map {
      case (_, t) if t =:= paramType => tq"_root_.dali.higher.Param1"
      case (_, t) if hasParamType(t, paramType) =>
        val lambdaTypeName = TypeName(c.freshName("Lambda$"))
        val newParamName = TypeName(c.freshName("A$"))
        val t1 = replaceParamType(t, paramType, newParamName)
        val lambda = tq"{type $lambdaTypeName[$newParamName] = $t1}"
        tq"_root_.dali.higher.Rec1[($lambda)#$lambdaTypeName]"
      case (_, t) => tq"_root_.dali.higher.Const1[$t]"
    }
    val reprType = reprTypes.foldRight(tq"_root_.dali.higher.HNil1": Tree) {
      case (t, acc) => tq"_root_.dali.higher.:**:[$t, $acc]"
    }

    val argName = TermName(c.freshName("a$"))
    val newParamName = TypeName(c.freshName("A$"))
    val newTypeA = replaceParamType(typeA, paramType, newParamName)

    val className = TypeName(c.freshName("Product1$"))
    q"""
      final class $className extends _root_.dali.higher.Generic1[$typeCons] {
        type Repr1 = $reprType
        def embed[$newParamName]($argName: $newTypeA): Repr1#Apply[$newParamName] =
          $argName match { case $pattern => $reprCons }
        def project[$newParamName]($argName: Repr1#Apply[$newParamName]): $newTypeA =
          $argName match { case $reprPattern => $cons }
      }
      new $className: _root_.dali.higher.Generic1.Aux[$typeCons, $reprType]
    """
  }

  private[this] def materializeCoproduct1(typeCons: Type, paramType: Type, typeA: Type): Tree = {
    val children = childrenOf(typeA)

    val reprs = children.map { t =>
      val lambdaTypeName = TypeName(c.freshName("Λ$"))
      val newParamName = TypeName(c.freshName("A$"))
      val t1 = replaceParamType(t, paramType, newParamName)
      tq"_root_.dali.higher.Rec1[({type $lambdaTypeName[$newParamName] = $t1})#$lambdaTypeName]"
    }

    val argName = TermName(c.freshName("a$"))
    val newParamName = TypeName(c.freshName("A$"))
    val newTypeA = replaceParamType(typeA, paramType, newParamName)
    val newChildren = children.map(child => replaceParamType(child, paramType, newParamName))

    val reprType = reprs.foldRight(tq"_root_.dali.higher.CNil1": Tree) {
      case (t, acc) => tq"_root_.dali.higher.:++:[$t, $acc]"
    }
    val cases = newChildren.zipWithIndex.map { case (t, i) => cq"_: $t => $i" }

    val className = TypeName(c.freshName("Coproduct1$"))
    q"""
      final class $className extends _root_.dali.higher.Generic1[$typeCons] {
        type Repr1 = $reprType
        def embed[$newParamName]($argName: $newTypeA): Repr1#Apply[$newParamName] =
          _root_.dali.Coproduct.unsafeApply($argName match { case ..$cases }, $argName).asInstanceOf[Repr1#Apply[$newParamName]]
        def project[$newParamName]($argName: Repr1#Apply[$newParamName]): $newTypeA =
          _root_.dali.Coproduct.unsafeGet($argName).asInstanceOf[$newTypeA]
      }
      new $className: _root_.dali.higher.Generic1.Aux[$typeCons, $reprType]
    """
  }
}
