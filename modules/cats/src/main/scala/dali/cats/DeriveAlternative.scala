package dali
package cats

import higher._

import _root_.cats.{Alternative, Applicative, MonoidK}

trait GAlternative[R <: TypeFunction1] extends GApplicative[R] with GMonoidK[R]

object GAlternative {
  @inline def apply[R <: TypeFunction1: GAlternative]: GAlternative[R] = implicitly

  implicit def hcons[H <: TypeFunction1: GAlternative, T <: HList1: GAlternative]: GAlternative[H :**: T] =
    new GAlternativeHCons[H, T] {
      val H = GAlternative[H]
      val T = GAlternative[T]
    }

  implicit val hnil: GAlternative[HNil1] = new GAlternativeHNil {}

  implicit def rec[F[_]](implicit F: => Alternative[F]): GAlternative[Rec1[F]] =
    new GAlternativeRec[F] {
      lazy val alternative = F
    }
}

private[cats] trait GAlternativeHCons[H <: TypeFunction1, T <: HList1]
    extends GAlternative[H :**: T]
    with GApplicativeHCons[H, T]
    with GMonoidKHCons[H, T] {
  val H: GAlternative[H]
  val T: GAlternative[T]
}

private[cats] trait GAlternativeHNil extends GAlternative[HNil1] with GApplicativeHNil with GMonoidKHNil

private[cats] trait GAlternativeRec[F[_]] extends GAlternative[Rec1[F]] with GApplicativeRec[F] with GMonoidKRec[F] {
  val alternative: Alternative[F]
  lazy val applicative: Applicative[F] = alternative
  lazy val monoidK: MonoidK[F] = alternative
}

trait DerivedAlternative[F[_], R <: TypeFunction1]
    extends Alternative[F]
    with DerivedApplicative[F, R]
    with DerivedMonoidK[F, R] {
  val galternative: GAlternative[R]
  lazy val gapplicative: GApplicative[R] = galternative
  lazy val gmonoidK: GMonoidK[R] = galternative
}

trait DeriveAlternative {
  implicit def deriveAlternative[F[_], R <: TypeFunction1](implicit FR: Generic1.Aux[F, R],
                                                           R: GAlternative[R]): Alternative[F] =
    new DerivedAlternative[F, R] {
      val generic1 = FR
      val galternative = R
    }
}
