package dali
package cats

import higher._

import _root_.cats.SemigroupK
import _root_.cats.kernel.Semigroup

trait GSemigroupK[R <: TypeFunction1] {
  def gcombineK[A](x: R#Apply[A], y: R#Apply[A]): R#Apply[A]
}

object GSemigroupK {
  @inline def apply[R <: TypeFunction1: GSemigroupK]: GSemigroupK[R] = implicitly

  implicit def hcons[H <: TypeFunction1: GSemigroupK, T <: HList1: GSemigroupK]: GSemigroupK[H :**: T] =
    new GSemigroupKHCons[H, T] {
      val H = GSemigroupK[H]
      val T = GSemigroupK[T]
    }

  implicit val hnil: GSemigroupK[HNil1] = new GSemigroupKHNil {}

  implicit def const[K: Semigroup]: GSemigroupK[Const1[K]] = new GSemigroupKConst[K] {
    val K = Semigroup[K]
  }

  implicit def rec[F[_]](implicit F: => SemigroupK[F]): GSemigroupK[Rec1[F]] =
    new GSemigroupRec[F] {
      lazy val semigroupK = F
    }
}

private[cats] trait GSemigroupKHCons[H <: TypeFunction1, T <: HList1] extends GSemigroupK[H :**: T] {
  val H: GSemigroupK[H]
  val T: GSemigroupK[T]

  def gcombineK[A](x: (H :**: T)#Apply[A], y: (H :**: T)#Apply[A]): (H :**: T)#Apply[A] =
    H.gcombineK(x.head, y.head) :*: T.gcombineK(x.tail, y.tail)
}

private[cats] trait GSemigroupKHNil extends GSemigroupK[HNil1] {
  def gcombineK[A](x: HNil, y: HNil): HNil = HNil
}

private[cats] trait GSemigroupKConst[K] extends GSemigroupK[Const1[K]] {
  val K: Semigroup[K]

  def gcombineK[A](x: K, y: K): K = K.combine(x, y)
}

private[cats] trait GSemigroupRec[F[_]] extends GSemigroupK[Rec1[F]] {
  val semigroupK: SemigroupK[F]

  def gcombineK[A](x: F[A], y: F[A]): F[A] = semigroupK.combineK(x, y)
}

trait DerivedSemigroupK[F[_], R <: TypeFunction1] extends SemigroupK[F] {
  val generic1: Generic1.Aux[F, R]
  val gsemigroupK: GSemigroupK[R]

  def combineK[A](x: F[A], y: F[A]): F[A] =
    generic1.project(gsemigroupK.gcombineK(generic1.embed(x), generic1.embed(y)))
}

trait DeriveSemigroupK {
  implicit def deriveSemigroupK[F[_], R <: TypeFunction1](implicit FR: Generic1.Aux[F, R],
                                                          R: GSemigroupK[R]): SemigroupK[F] =
    new DerivedSemigroupK[F, R] {
      val generic1 = FR
      val gsemigroupK = R
    }
}
