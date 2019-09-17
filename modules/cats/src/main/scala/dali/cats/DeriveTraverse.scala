package dali
package cats

import higher._

import _root_.cats.{Applicative, Eval, Traverse}

trait GTraverse[R <: TypeFunction1] extends GFunctor[R] with GFoldable[R] {
  def gtraverse[G[_]: Applicative, A, B](ra: R#Apply[A])(f: A => Eval[G[B]]): Eval[G[R#Apply[B]]]
}

object GTraverse {
  @inline def apply[R <: TypeFunction1: GTraverse]: GTraverse[R] = implicitly

  implicit def hcons[H <: TypeFunction1: GTraverse, T <: HList1: GTraverse]: GTraverse[H :**: T] =
    new GTraverseHCons[H, T] {
      val H = GTraverse[H]
      val T = GTraverse[T]
    }

  implicit val hnil: GTraverse[HNil1] = new GTraverseHNil {}

  implicit def ccons[H <: TypeFunction1: GTraverse, T <: Coproduct1: GTraverse]: GTraverse[H :++: T] =
    new GTraverseCCons[H, T] {
      val H = GTraverse[H]
      val T = GTraverse[T]
    }

  implicit val cnil: GTraverse[CNil1] = new GTraverseCNil {}

  implicit def const[K]: GTraverse[Const1[K]] = new GTraverseConst[K] {}

  implicit val param: GTraverse[Param1] = new GTraverseParam {}

  implicit def rec[F[_]](implicit F: => Traverse[F]): GTraverse[Rec1[F]] =
    new GTraverseRec[F] {
      lazy val traverse = F
    }
}

private[cats] trait GTraverseHCons[H <: TypeFunction1, T <: HList1]
    extends GTraverse[H :**: T]
    with GFunctorHCons[H, T]
    with GFoldableHCons[H, T] {
  val H: GTraverse[H]
  val T: GTraverse[T]

  def gtraverse[G[_]: Applicative, A, B](ra: (H :**: T)#Apply[A])(f: A => Eval[G[B]]): Eval[G[(H :**: T)#Apply[B]]] =
    ra match {
      case h :*: t =>
        for {
          h1 <- H.gtraverse(h)(f)
          t1 <- T.gtraverse(t)(f)
        } yield Applicative[G].map2(h1, t1)(_ :*: _)
    }
}

private[cats] trait GTraverseHNil extends GTraverse[HNil1] with GFunctorHNil with GFoldableHNil {
  def gtraverse[G[_]: Applicative, A, B](ra: HNil)(f: A => Eval[G[B]]): Eval[G[HNil]] =
    Eval.now(Applicative[G].pure(HNil))
}

private[cats] trait GTraverseCCons[H <: TypeFunction1, T <: Coproduct1]
    extends GTraverse[H :++: T]
    with GFunctorCCons[H, T]
    with GFoldableCCons[H, T] {
  val H: GTraverse[H]
  val T: GTraverse[T]

  def gtraverse[G[_]: Applicative, A, B](ra: (H :++: T)#Apply[A])(f: A => Eval[G[B]]): Eval[G[(H :++: T)#Apply[B]]] =
    ra match {
      case Inl(h) => H.gtraverse(h)(f).map(Applicative[G].map(_)(Inl(_)))
      case Inr(t) => T.gtraverse(t)(f).map(Applicative[G].map(_)(Inr(_)))
    }
}

private[cats] trait GTraverseCNil extends GTraverse[CNil1] with GFunctorCNil with GFoldableCNil {
  def gtraverse[G[_]: Applicative, A, B](ra: CNil)(f: A => Eval[G[B]]): Eval[G[CNil]] = ???
}

private[cats] trait GTraverseConst[K] extends GTraverse[Const1[K]] with GFunctorConst[K] with GFoldableConst[K] {
  def gtraverse[G[_]: Applicative, A, B](ra: K)(f: A => Eval[G[B]]): Eval[G[K]] =
    Eval.now(Applicative[G].pure(ra))
}

private[cats] trait GTraverseParam extends GTraverse[Param1] with GFunctorParam with GFoldableParam {
  def gtraverse[G[_]: Applicative, A, B](ra: A)(f: A => Eval[G[B]]): Eval[G[B]] = f(ra)
}

private[cats] trait GTraverseRec[F[_]] extends GTraverse[Rec1[F]] with GFunctorRec[F] with GFoldableRec[F] {
  val traverse: Traverse[F]
  override lazy val functor = traverse
  override lazy val foldable = traverse

  def gtraverse[G[_]: Applicative, A, B](ra: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]] =
    traverse.traverse[Lambda[T => Eval[G[T]]], A, B](ra)(f)(Applicative[Eval].compose[G])
}

trait DerivedTraverse[F[_], R <: TypeFunction1]
    extends Traverse[F]
    with DerivedFunctor[F, R]
    with DerivedFoldable[F, R] {
  val gtraverse: GTraverse[R]
  override lazy val gfunctor: GFunctor[R] = gtraverse
  override lazy val gfoldable: GFoldable[R] = gtraverse

  override def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    gtraverse.gtraverse(generic1.embed(fa))(a => Eval.later(f(a))).map(Applicative[G].map(_)(generic1.project)).value
}

trait DeriveTraverse {
  implicit def deriveTraverse[F[_], R <: TypeFunction1](implicit FR: Generic1.Aux[F, R], R: GTraverse[R]): Traverse[F] =
    new DerivedTraverse[F, R] {
      val generic1 = FR
      val gtraverse = R
    }
}
