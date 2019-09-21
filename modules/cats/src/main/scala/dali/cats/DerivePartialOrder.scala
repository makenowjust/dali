package dali
package cats

import _root_.cats.{Eq, PartialOrder}

trait GPartialOrder[R] extends GEq[R] {
  def gpartialCompare(x: R, y: R): Double
}

object GPartialOrder {
  @inline def apply[R: GPartialOrder]: GPartialOrder[R] = implicitly

  implicit def hcons[H: GPartialOrder, T <: HList: GPartialOrder]: GPartialOrder[H :*: T] =
    new GPartialOrderHCons[H, T] {
      val H: GPartialOrder[H] = GPartialOrder[H]
      val T: GPartialOrder[T] = GPartialOrder[T]
    }

  implicit def hnil: GPartialOrder[HNil] = new GPartialOrderHNil {}

  implicit def ccons[H: GPartialOrder, T <: Coproduct: GPartialOrder]: GPartialOrder[H :+: T] =
    new GPartialOrderCCons[H, T] {
      val H: GPartialOrder[H] = GPartialOrder[H]
      val T: GPartialOrder[T] = GPartialOrder[T]
    }

  implicit def cnil: GPartialOrder[CNil] = new GPartialOrderCNil {}

  implicit def value[A](implicit A: => PartialOrder[A]): GPartialOrder[A] =
    new GPartialOrderValue[A] {
      lazy val partialOrder: PartialOrder[A] = A
    }
}

private[cats] trait GPartialOrderHCons[H, T <: HList] extends GPartialOrder[H :*: T] with GEqHCons[H, T] {
  val H: GPartialOrder[H]
  val T: GPartialOrder[T]

  def gpartialCompare(x: H :*: T, y: H :*: T): Double = {
    val cmp = H.gpartialCompare(x.head, y.head)
    if (cmp == 0) T.gpartialCompare(x.tail, y.tail) else cmp
  }
}

private[cats] trait GPartialOrderHNil extends GPartialOrder[HNil] with GEqHNil {
  def gpartialCompare(x: HNil, y: HNil): Double = 0
}

private[cats] trait GPartialOrderCCons[H, T <: Coproduct] extends GPartialOrder[H :+: T] with GEqCCons[H, T] {
  val H: GPartialOrder[H]
  val T: GPartialOrder[T]

  def gpartialCompare(x: H :+: T, y: H :+: T): Double = (x, y) match {
    case (Inl(xh), Inl(yh)) => H.gpartialCompare(xh, yh)
    case (Inr(xt), Inr(yt)) => T.gpartialCompare(xt, yt)
    case _                  => Double.NaN
  }
}

private[cats] trait GPartialOrderCNil extends GPartialOrder[CNil] with GEqCNil {
  def gpartialCompare(x: CNil, y: CNil): Double = ???
}

private[cats] trait GPartialOrderValue[A] extends GPartialOrder[A] with GEqValue[A] {
  val partialOrder: PartialOrder[A]
  override lazy val eq: Eq[A] = partialOrder

  def gpartialCompare(x: A, y: A): Double = partialOrder.partialCompare(x, y)
}

trait DerivedPartialOrder[A, R] extends PartialOrder[A] with DerivedEq[A, R] {
  val gpartialOrder: GPartialOrder[R]
  override lazy val geq: GEq[R] = gpartialOrder

  override def partialCompare(x: A, y: A): Double =
    gpartialOrder.gpartialCompare(generic.embed(x), generic.embed(y))
}

trait DerivePartialOrder {
  implicit def derivePartialOrder[A, R](implicit AR: Generic.Aux[A, R], R: GPartialOrder[R]): PartialOrder[A] =
    new DerivedPartialOrder[A, R] {
      val generic: Generic.Aux[A, R] = AR
      val gpartialOrder: GPartialOrder[R] = R
    }
}
