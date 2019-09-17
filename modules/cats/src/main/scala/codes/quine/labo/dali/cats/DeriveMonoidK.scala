package codes.quine.labo
package dali
package cats

import higher._

import _root_.cats.{MonoidK, SemigroupK}
import _root_.cats.kernel.Monoid

trait GMonoidK[R <: TypeFunction1] extends GSemigroupK[R] {
  def gempty[A]: R#Apply[A]
}

object GMonoidK {
  @inline def apply[R <: TypeFunction1: GMonoidK]: GMonoidK[R] = implicitly

  implicit def hcons[H <: TypeFunction1: GMonoidK, T <: HList1: GMonoidK]: GMonoidK[H :**: T] =
    new GMonoidKHCons[H, T] {
      val H = GMonoidK[H]
      val T = GMonoidK[T]
    }

  implicit val hnil: GMonoidK[HNil1] = new GMonoidKHNil {}

  implicit def const[K: Monoid]: GMonoidK[Const1[K]] = new GMonoidKConst[K] {
    val K = Monoid[K]
  }

  implicit def rec[F[_]](implicit F: => MonoidK[F]): GMonoidK[Rec1[F]] = new GMonoidKRec[F] {
    lazy val monoidK = F
  }
}

private[cats] trait GMonoidKHCons[H <: TypeFunction1, T <: HList1]
    extends GMonoidK[H :**: T]
    with GSemigroupKHCons[H, T] {
  val H: GMonoidK[H]
  val T: GMonoidK[T]

  def gempty[A]: (H :**: T)#Apply[A] = H.gempty[A] :*: T.gempty
}

private[cats] trait GMonoidKHNil extends GMonoidK[HNil1] with GSemigroupKHNil {
  def gempty[A]: HNil = HNil
}

private[cats] trait GMonoidKConst[K] extends GMonoidK[Const1[K]] with GSemigroupKConst[K] {
  val K: Monoid[K]

  def gempty[A]: K = K.empty
}

private[cats] trait GMonoidKRec[F[_]] extends GMonoidK[Rec1[F]] with GSemigroupRec[F] {
  val monoidK: MonoidK[F]
  lazy val semigroupK: SemigroupK[F] = monoidK

  def gempty[A]: F[A] = monoidK.empty
}

trait DerivedMonoidK[F[_], R <: TypeFunction1] extends MonoidK[F] with DerivedSemigroupK[F, R] {
  val gmonoidK: GMonoidK[R]
  lazy val gsemigroupK: GSemigroupK[R] = gmonoidK

  def empty[A]: F[A] = generic1.project(gmonoidK.gempty)
}

trait DeriveMonoidK {
  implicit def deriveMonoidK[F[_], R <: TypeFunction1](implicit FR: Generic1.Aux[F, R], R: GMonoidK[R]): MonoidK[F] =
    new DerivedMonoidK[F, R] {
      val generic1 = FR
      val gmonoidK = R
    }
}
