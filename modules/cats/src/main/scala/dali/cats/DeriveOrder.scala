package dali
package cats

import _root_.cats.{Order, PartialOrder}

trait GOrder[R] extends GPartialOrder[R] {
  def gcompare(x: R, y: R): Int
}

object GOrder {
  @inline def apply[R: GOrder]: GOrder[R] = implicitly

  implicit def hcons[H: GOrder, T <: HList: GOrder]: GOrder[H :*: T] =
    new GOrderHCons[H, T] {
      val H: GOrder[H] = GOrder[H]
      val T: GOrder[T] = GOrder[T]
    }

  implicit def hnil: GOrder[HNil] = new GOrderHNil {}

  implicit def value[A](implicit A: => Order[A]): GOrder[A] = new GOrderValue[A] {
    lazy val order: Order[A] = A
  }
}

private[cats] trait GOrderHCons[H, T <: HList] extends GOrder[H :*: T] with GPartialOrderHCons[H, T] {
  val H: GOrder[H]
  val T: GOrder[T]

  def gcompare(x: H :*: T, y: H :*: T): Int = {
    val cmp = H.gcompare(x.head, y.head)
    if (cmp == 0) T.gcompare(x.tail, y.tail) else cmp
  }
}

private[cats] trait GOrderHNil extends GOrder[HNil] with GPartialOrderHNil {
  def gcompare(x: HNil, y: HNil): Int = 0
}

private[cats] trait GOrderValue[A] extends GOrder[A] with GPartialOrderValue[A] {
  val order: Order[A]
  override lazy val partialOrder: PartialOrder[A] = order

  def gcompare(x: A, y: A): Int = order.compare(x, y)
}

trait DerivedOrder[A, R] extends Order[A] with DerivedPartialOrder[A, R] {
  val gorder: GOrder[R]
  lazy val gpartialOrder: GPartialOrder[R] = gorder

  def compare(x: A, y: A): Int = gorder.gcompare(generic.embed(x), generic.embed(y))
}

trait DeriveOrder {
  implicit def deriveOrder[A, R](implicit AR: Generic.Aux[A, R], R: GOrder[R]): Order[A] =
    new DerivedOrder[A, R] {
      val generic: Generic.Aux[A, R] = AR
      val gorder: GOrder[R] = R
    }
}
