package codes.quine.labo
package dali
package cats

import higher._

import _root_.cats.{Applicative, FlatMap, Monad}

trait GMonad[R <: TypeFunction1] extends GFlatMap[R] with GApplicative[R]

object GMonad {
  @inline def apply[R <: TypeFunction1: GMonad]: GMonad[R] = implicitly

  implicit def hcons[H <: TypeFunction1: GMonad, T <: HList1: GMonad]: GMonad[H :**: T] =
    new GMonadHCons[H, T] {
      val H = GMonad[H]
      val T = GMonad[T]
    }

  implicit def hnil: GMonad[HNil1] = new GMonadHNil {}

  implicit def param: GMonad[Param1] = new GMonadParam {}

  implicit def rec[F[_]](implicit F: => Monad[F]): GMonad[Rec1[F]] =
    new GMonadRec[F] {
      lazy val monad = F
    }
}

private[cats] trait GMonadHCons[H <: TypeFunction1, T <: HList1]
    extends GMonad[H :**: T]
    with GFlatMapHCons[H, T]
    with GApplicativeHCons[H, T] {
  val H: GMonad[H]
  val T: GMonad[T]
}

private[cats] trait GMonadHNil extends GMonad[HNil1] with GFlatMapHNil with GApplicativeHNil

private[cats] trait GMonadParam extends GMonad[Param1] with GFlatMapParam with GApplicativeParam

private[cats] trait GMonadRec[F[_]] extends GMonad[Rec1[F]] with GFlatMapRec[F] with GApplicativeRec[F] {
  val monad: Monad[F]
  override lazy val applicative: Applicative[F] = monad
  override lazy val flatMap: FlatMap[F] = monad
}

trait DerivedMonad[F[_], R <: TypeFunction1] extends Monad[F] with DerivedFlatMap[F, R] with DerivedApplicative[F, R] {
  val gmonad: GMonad[R]
  override lazy val gapplicative: GApplicative[R] = gmonad
  override lazy val gflatMap: GFlatMap[R] = gmonad
}

trait DeriveMonad {
  implicit def deriveMonad[F[_], R <: TypeFunction1](implicit FR: Generic1.Aux[F, R], R: GMonad[R]): Monad[F] =
    new DerivedMonad[F, R] {
      val generic1 = FR
      val gmonad = R
    }
}
