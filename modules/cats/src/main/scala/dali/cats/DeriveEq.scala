package dali
package cats

import _root_.cats.Eq

trait GEq[R] {
  def geqv(x: R, y: R): Boolean
}

object GEq {
  @inline def apply[R: GEq]: GEq[R] = implicitly

  implicit def hcons[H: GEq, T <: HList: GEq]: GEq[H :*: T] =
    new GEqHCons[H, T] {
      val H: GEq[H] = GEq[H]
      val T: GEq[T] = GEq[T]
    }

  implicit def hnil: GEq[HNil] = new GEqHNil {}

  implicit def ccons[H: GEq, T <: Coproduct: GEq]: GEq[H :+: T] =
    new GEqCCons[H, T] {
      val H: GEq[H] = GEq[H]
      val T: GEq[T] = GEq[T]
    }

  implicit def cnil: GEq[CNil] = new GEqCNil {}

  implicit def value[A](implicit A: => Eq[A]): GEq[A] =
    new GEqValue[A] {
      lazy val eq: Eq[A] = A
    }
}

private[cats] trait GEqHCons[H, T <: HList] extends GEq[H :*: T] {
  val H: GEq[H]
  val T: GEq[T]

  def geqv(x: H :*: T, y: H :*: T): Boolean =
    H.geqv(x.head, y.head) && T.geqv(x.tail, y.tail)
}

private[cats] trait GEqHNil extends GEq[HNil] {
  def geqv(x: HNil, y: HNil): Boolean = true
}

private[cats] trait GEqCCons[H, T <: Coproduct] extends GEq[H :+: T] {
  val H: GEq[H]
  val T: GEq[T]

  def geqv(x: H :+: T, y: H :+: T): Boolean = (x, y) match {
    case (Inl(xh), Inl(yh)) => H.geqv(xh, yh)
    case (Inr(xt), Inr(yt)) => T.geqv(xt, yt)
    case _                  => false
  }
}

private[cats] trait GEqCNil extends GEq[CNil] {
  def geqv(x: CNil, y: CNil): Boolean = ???
}

private[cats] trait GEqValue[A] extends GEq[A] {
  val eq: Eq[A]

  def geqv(x: A, y: A): Boolean = eq.eqv(x, y)
}

trait DerivedEq[A, R] extends Eq[A] {
  val generic: Generic.Aux[A, R]
  val geq: GEq[R]

  override def eqv(x: A, y: A): Boolean =
    geq.geqv(generic.embed(x), generic.embed(y))
}

trait DeriveEq {
  implicit def deriveEq[A, R](implicit AR: Generic.Aux[A, R], R: GEq[R]): Eq[A] =
    new DerivedEq[A, R] {
      val generic: Generic.Aux[A, R] = AR
      lazy val geq: GEq[R] = R
    }
}
