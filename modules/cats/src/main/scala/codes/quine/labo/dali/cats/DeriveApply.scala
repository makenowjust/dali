package codes.quine.labo
package dali
package cats

import higher._

import _root_.cats.{Apply, Functor}
import _root_.cats.kernel.Semigroup

trait GApply[R <: TypeFunction1] extends GFunctor[R] {
  def gap[A, B](rf: R#Apply[A => B])(ra: R#Apply[A]): R#Apply[B]
}

object GApply {
  @inline def apply[R <: TypeFunction1: GApply]: GApply[R] = implicitly

  implicit def hcons[H <: TypeFunction1: GApply, T <: HList1: GApply]: GApply[H :**: T] =
    new GApplyHCons[H, T] {
      val H = GApply[H]
      val T = GApply[T]
    }

  implicit val hnil: GApply[HNil1] = new GApplyHNil {}

  implicit def const[K: Semigroup]: GApply[Const1[K]] = new GApplyConst[K] {
    val K = Semigroup[K]
  }

  implicit val param: GApply[Param1] = new GApplyParam {}

  implicit def rec[F[_]](implicit F: => Apply[F]): GApply[Rec1[F]] =
    new GApplyRec[F] {
      lazy val apply = F
    }
}

private[cats] trait GApplyHCons[H <: TypeFunction1, T <: HList1] extends GApply[H :**: T] with GFunctorHCons[H, T] {
  val H: GApply[H]
  val T: GApply[T]

  def gap[A, B](rf: (H :**: T)#Apply[A => B])(ra: (H :**: T)#Apply[A]): (H :**: T)#Apply[B] = {
    val fh :*: ft = rf
    val ah :*: at = ra
    H.gap(fh)(ah) :*: T.gap(ft)(at)
  }
}

private[cats] trait GApplyHNil extends GApply[HNil1] with GFunctorHNil {
  def gap[A, B](rf: HNil)(ra: HNil): HNil = HNil
}

private[cats] trait GApplyConst[K] extends GApply[Const1[K]] with GFunctorConst[K] {
  val K: Semigroup[K]

  def gap[A, B](rf: K)(ra: K): K = K.combine(rf, ra)
}

private[cats] trait GApplyParam extends GApply[Param1] with GFunctorParam {
  def gap[A, B](rf: A => B)(ra: A): B = rf(ra)
}

private[cats] trait GApplyRec[F[_]] extends GApply[Rec1[F]] with GFunctorRec[F] {
  val apply: Apply[F]
  override lazy val functor: Functor[F] = apply

  def gap[A, B](rf: F[A => B])(ra: F[A]): F[B] = apply.ap(rf)(ra)
}

trait DerivedApply[F[_], R <: TypeFunction1] extends Apply[F] with DerivedFunctor[F, R] {
  val gapply: GApply[R]
  override lazy val gfunctor: GFunctor[R] = gapply

  override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    generic1.project(gapply.gap(generic1.embed(ff))(generic1.embed(fa)))
}

trait DeriveApply {
  implicit def deriveApply[F[_], R <: TypeFunction1](implicit FR: Generic1.Aux[F, R], R: GApply[R]): Apply[F] =
    new DerivedApply[F, R] {
      val generic1 = FR
      val gapply = R
    }
}
