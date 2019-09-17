package codes.quine.labo
package dali
package cats

import higher._

import _root_.cats.{Eval, Foldable}

trait GFoldable[R <: TypeFunction1] {
  def gfoldLeft[A, B](ra: R#Apply[A], b: B)(f: (B, A) => Eval[B]): Eval[B]
  def gfoldRight[A, B](ra: R#Apply[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
}

object GFoldable {
  @inline def apply[R <: TypeFunction1: GFoldable]: GFoldable[R] = implicitly

  implicit def hcons[H <: TypeFunction1: GFoldable, T <: HList1: GFoldable]: GFoldable[H :**: T] =
    new GFoldableHCons[H, T] {
      val H = GFoldable[H]
      val T = GFoldable[T]
    }

  implicit val hnil: GFoldable[HNil1] = new GFoldableHNil {}

  implicit def ccons[H <: TypeFunction1: GFoldable, T <: Coproduct1: GFoldable]: GFoldable[H :++: T] =
    new GFoldableCCons[H, T] {
      val H = GFoldable[H]
      val T = GFoldable[T]
    }

  implicit val cnil: GFoldable[CNil1] = new GFoldableCNil {}

  implicit def const[K]: GFoldable[Const1[K]] = new GFoldableConst[K] {}

  implicit val param: GFoldable[Param1] = new GFoldableParam {}

  implicit def rec[F[_]](implicit F: => Foldable[F]): GFoldable[Rec1[F]] =
    new GFoldableRec[F] {
      lazy val foldable = F
    }
}

private[cats] trait GFoldableHCons[H <: TypeFunction1, T <: HList1] extends GFoldable[H :**: T] {
  val H: GFoldable[H]
  val T: GFoldable[T]

  def gfoldLeft[A, B](ra: (H :**: T)#Apply[A], b: B)(f: (B, A) => Eval[B]): Eval[B] =
    ra match {
      case h :*: t => H.gfoldLeft(h, b)(f).flatMap(T.gfoldLeft(t, _)(f))
    }

  def gfoldRight[A, B](ra: (H :**: T)#Apply[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    ra match {
      case h :*: t => H.gfoldRight(h, Eval.defer(T.gfoldRight(t, lb)(f)))(f)
    }
}

private[cats] trait GFoldableHNil extends GFoldable[HNil1] {
  def gfoldLeft[A, B](ra: HNil, b: B)(f: (B, A) => Eval[B]): Eval[B] = Eval.now(b)
  def gfoldRight[A, B](ra: HNil, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
}

private[cats] trait GFoldableCCons[H <: TypeFunction1, T <: Coproduct1] extends GFoldable[H :++: T] {
  val H: GFoldable[H]
  val T: GFoldable[T]

  def gfoldLeft[A, B](ra: (H :++: T)#Apply[A], b: B)(f: (B, A) => Eval[B]): Eval[B] =
    ra match {
      case Inl(h) => H.gfoldLeft(h, b)(f)
      case Inr(t) => T.gfoldLeft(t, b)(f)
    }

  def gfoldRight[A, B](ra: (H :++: T)#Apply[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    ra match {
      case Inl(h) => H.gfoldRight(h, lb)(f)
      case Inr(t) => T.gfoldRight(t, lb)(f)
    }
}

private[cats] trait GFoldableCNil extends GFoldable[CNil1] {
  def gfoldLeft[A, B](ra: CNil, b: B)(f: (B, A) => Eval[B]): Eval[B] = ???
  def gfoldRight[A, B](ra: CNil, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
}

private[cats] trait GFoldableConst[K] extends GFoldable[Const1[K]] {
  def gfoldLeft[A, B](ra: K, b: B)(f: (B, A) => Eval[B]): Eval[B] = Eval.now(b)
  def gfoldRight[A, B](ra: K, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
}

private[cats] trait GFoldableParam extends GFoldable[Param1] {
  def gfoldLeft[A, B](ra: A, b: B)(f: (B, A) => Eval[B]): Eval[B] = f(b, ra)
  def gfoldRight[A, B](ra: A, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = Eval.defer(f(ra, lb))
}

private[cats] trait GFoldableRec[F[_]] extends GFoldable[Rec1[F]] {
  val foldable: Foldable[F]

  def gfoldLeft[A, B](ra: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B] = foldable.foldLeftM(ra, b)(f)
  def gfoldRight[A, B](ra: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = foldable.foldRight(ra, lb)(f)
}

trait DerivedFoldable[F[_], R <: TypeFunction1] extends Foldable[F] {
  val generic1: Generic1.Aux[F, R]
  val gfoldable: GFoldable[R]

  override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    gfoldable.gfoldLeft(generic1.embed(fa), b)((b, a) => Eval.later(f(b, a))).value

  override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    gfoldable.gfoldRight(generic1.embed(fa), lb)(f)
}

trait DeriveFoldable {
  implicit def deriveFoldable[F[_], R <: TypeFunction1](implicit FR: Generic1.Aux[F, R], R: GFoldable[R]): Foldable[F] =
    new DerivedFoldable[F, R] {
      val generic1 = FR
      val gfoldable = R
    }
}
