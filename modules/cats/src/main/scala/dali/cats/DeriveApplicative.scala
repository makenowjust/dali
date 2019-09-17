package dali
package cats

import higher._

import _root_.cats.{Applicative, Apply}
import _root_.cats.kernel.Monoid

trait GApplicative[R <: TypeFunction1] extends GApply[R] {
  def gpure[A](a: A): R#Apply[A]
}

object GApplicative {
  def apply[R <: TypeFunction1: GApplicative]: GApplicative[R] = implicitly

  implicit def hcons[H <: TypeFunction1: GApplicative, T <: HList1: GApplicative]: GApplicative[H :**: T] =
    new GApplicativeHCons[H, T] {
      val H = GApplicative[H]
      val T = GApplicative[T]
    }

  implicit val hnil: GApplicative[HNil1] = new GApplicativeHNil {}

  implicit def const[K: Monoid]: GApplicative[Const1[K]] = new GApplicativeConst[K] {
    val K = Monoid[K]
  }

  implicit val param: GApplicative[Param1] = new GApplicativeParam {}

  implicit def rec[F[_]](implicit F: => Applicative[F]): GApplicative[Rec1[F]] = new GApplicativeRec[F] {
    lazy val applicative = F
  }
}

private[cats] trait GApplicativeHCons[H <: TypeFunction1, T <: HList1]
    extends GApplicative[H :**: T]
    with GApplyHCons[H, T] {
  val H: GApplicative[H]
  val T: GApplicative[T]

  def gpure[A](a: A): (H :**: T)#Apply[A] = H.gpure(a) :*: T.gpure(a)
}

private[cats] trait GApplicativeHNil extends GApplicative[HNil1] with GApplyHNil {
  def gpure[A](a: A): HNil = HNil
}

private[cats] trait GApplicativeConst[K] extends GApplicative[Const1[K]] with GApplyConst[K] {
  val K: Monoid[K]

  def gpure[A](a: A): K = K.empty
}

private[cats] trait GApplicativeParam extends GApplicative[Param1] with GApplyParam {
  def gpure[A](a: A): A = a
}

private[cats] trait GApplicativeRec[F[_]] extends GApplicative[Rec1[F]] with GApplyRec[F] {
  val applicative: Applicative[F]
  override lazy val apply: Apply[F] = applicative

  def gpure[A](a: A): F[A] = applicative.pure(a)
}

trait DerivedApplicative[F[_], R <: TypeFunction1] extends Applicative[F] with DerivedApply[F, R] {
  val gapplicative: GApplicative[R]
  override lazy val gapply: GApply[R] = gapplicative

  def pure[A](a: A): F[A] = generic1.project(gapplicative.gpure(a))
}

trait DeriveApplicative {
  implicit def deriveApplicative[F[_], R <: TypeFunction1](implicit FR: Generic1.Aux[F, R],
                                                           R: GApplicative[R]): Applicative[F] =
    new DerivedApplicative[F, R] {
      val generic1 = FR
      val gapplicative = R
    }
}
