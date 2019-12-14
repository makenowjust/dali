package dali
package singleton
package macros

import scala.reflect.macros.whitebox
import Value._

class ValuableMacros(private[singleton] val c: whitebox.Context) {
  import c.universe._

  private[this] case class Unary(op: Type) {
    def unapply(e: Type): Option[Type] =
      e match {
        case TypeRef(_, sym, List(op0, e)) if sym == unary && op0 =:= op => Some(e)
        case _                                                           => None
      }
  }

  private[this] lazy val ToInt = Unary(opToInt)
  private[this] lazy val ToLong = Unary(opToLong)
  private[this] lazy val ToFloat = Unary(opToFloat)
  private[this] lazy val ToDouble = Unary(opToDouble)

  private[this] case class Binary(op: Type) {
    def unapply(e: Type): Option[(Type, Type)] =
      e match {
        case TypeRef(_, sym, List(op0, l, r)) if sym == binary && op0 =:= op => Some((l, r))
        case _                                                               => None
      }
  }

  private[this] lazy val Add = Binary(opAdd)
  private[this] lazy val Sub = Binary(opSub)
  private[this] lazy val Mul = Binary(opMul)
  private[this] lazy val Div = Binary(opDiv)
  private[this] lazy val Mod = Binary(opMod)
  private[this] lazy val Eq = Binary(opEq)

  private[this] lazy val unary: Symbol = typeOf[singleton.Expr.Unary[Op.Unary, Any]].typeSymbol
  private[this] lazy val opToInt: Type = typeOf[Op.ToInt]
  private[this] lazy val opToLong: Type = typeOf[Op.ToLong]
  private[this] lazy val opToFloat: Type = typeOf[Op.ToFloat]
  private[this] lazy val opToDouble: Type = typeOf[Op.ToDouble]
  private[this] lazy val binary: Symbol = typeOf[singleton.Expr.Binary[Op.Binary, Any, Any]].typeSymbol
  private[this] lazy val opAdd: Type = typeOf[Op.Add]
  private[this] lazy val opSub: Type = typeOf[Op.Sub]
  private[this] lazy val opMul: Type = typeOf[Op.Mul]
  private[this] lazy val opDiv: Type = typeOf[Op.Div]
  private[this] lazy val opMod: Type = typeOf[Op.Mod]
  private[this] lazy val opEq: Type = typeOf[Op.Eq]

  private[this] def abort(msg: String): Nothing =
    c.abort(c.enclosingPosition, msg)

  def evaluate[E: WeakTypeTag]: Tree = {
    val exprType = weakTypeOf[E]

    val v = try {
      evaluateType(exprType)
    } catch {
      case ex: ValueException => abort(ex.getMessage)
    }

    val t = valueToType(v)
    val l = valueToLiteral(v)

    val name = TypeName(c.freshName("evaluate$"))
    return q"""
      class $name extends _root_.dali.singleton.Valuable[$exprType] {
        type Value = $t
        def value: $t = $l
      }
      new $name: _root_.dali.singleton.Valuable.Aux[$exprType, $t]
    """
  }

  private[this] def evaluateType(e: Type): Value =
    e.dealias match {
      case ToInt(e)    => Value.toInt(evaluateType(e))
      case ToLong(e)   => Value.toLong(evaluateType(e))
      case ToFloat(e)  => Value.toFloat(evaluateType(e))
      case ToDouble(e) => Value.toDouble(evaluateType(e))
      case Add(l, r)   => Value.add(evaluateType(l), evaluateType(r))
      case Sub(l, r)   => Value.sub(evaluateType(l), evaluateType(r))
      case Mul(l, r)   => Value.mul(evaluateType(l), evaluateType(r))
      case Div(l, r)   => Value.div(evaluateType(l), evaluateType(r))
      case Mod(l, r)   => Value.mod(evaluateType(l), evaluateType(r))
      case Eq(l, r)    => Value.eq(evaluateType(l), evaluateType(r))
      case ConstantType(c) =>
        c.value match {
          case v: Int     => IntValue(v)
          case v: Long    => LongValue(v)
          case v: Float   => FloatValue(v)
          case v: Double  => DoubleValue(v)
          case v: Boolean => BooleanValue(v)
          case v: String  => StringValue(v)
          case _          => abort("unknown constant")
        }
      case _ => abort(s"unknown type: ${showRaw(e)}")
    }

  private[this] def valueToType(v: Value): Type = v match {
    case IntValue(v)     => c.internal.constantType(Constant(v))
    case LongValue(v)    => c.internal.constantType(Constant(v))
    case FloatValue(v)   => c.internal.constantType(Constant(v))
    case DoubleValue(v)  => c.internal.constantType(Constant(v))
    case BooleanValue(v) => c.internal.constantType(Constant(v))
    case StringValue(v)  => c.internal.constantType(Constant(v))
  }

  private[this] def valueToLiteral(v: Value): Tree = v match {
    case IntValue(v)     => q"$v"
    case LongValue(v)    => q"$v"
    case FloatValue(v)   => q"$v"
    case DoubleValue(v)  => q"$v"
    case BooleanValue(v) => q"$v"
    case StringValue(v)  => q"$v"
  }
}
