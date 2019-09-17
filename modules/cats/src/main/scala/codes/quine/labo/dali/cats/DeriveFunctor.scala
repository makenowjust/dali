package codes.quine.labo
package dali
package cats

import higher._

import _root_.cats.{Eval, Functor}

trait GFunctor[R <: TypeFunction1] extends Serializable {
  def gmap[A, B](ra: R#Apply[A])(f: A => Eval[B]): Eval[R#Apply[B]]
}

object GFunctor {
  @inline def apply[R <: TypeFunction1: GFunctor]: GFunctor[R] = implicitly

  implicit def hcons[H <: TypeFunction1: GFunctor, T <: HList1: GFunctor]: GFunctor[H :**: T] =
    new GFunctorHCons[H, T] {
      val H = GFunctor[H]
      val T = GFunctor[T]
    }

  implicit val hnil: GFunctor[HNil1] = new GFunctorHNil {}

  implicit def ccons[H <: TypeFunction1: GFunctor, T <: Coproduct1: GFunctor]: GFunctor[H :++: T] =
    new GFunctorCCons[H, T] {
      val H = GFunctor[H]
      val T = GFunctor[T]
    }

  implicit val cnil: GFunctor[CNil1] = new GFunctorCNil {}

  implicit def const[K]: GFunctor[Const1[K]] = new GFunctorConst[K] {}

  implicit val param: GFunctor[Param1] = new GFunctorParam {}

  implicit def rec[F[_]](implicit F: => Functor[F]): GFunctor[Rec1[F]] =
    new GFunctorRec[F] {
      lazy val functor = F
    }
}

private[cats] trait GFunctorHCons[H <: TypeFunction1, T <: HList1] extends GFunctor[H :**: T] {
  val H: GFunctor[H]
  val T: GFunctor[T]

  def gmap[A, B](ra: (H :**: T)#Apply[A])(f: A => Eval[B]): Eval[(H :**: T)#Apply[B]] =
    ra match {
      case h :*: t =>
        for {
          h1 <- H.gmap(h)(f)
          t1 <- T.gmap(t)(f)
        } yield h1 :*: t1
    }
}

private[cats] trait GFunctorHNil extends GFunctor[HNil1] {
  def gmap[A, B](ra: HNil)(f: A => Eval[B]): Eval[HNil] = Eval.now(HNil)
}

private[cats] trait GFunctorCCons[H <: TypeFunction1, T <: Coproduct1] extends GFunctor[H :++: T] {
  val H: GFunctor[H]
  val T: GFunctor[T]

  def gmap[A, B](ra: (H :++: T)#Apply[A])(f: A => Eval[B]): Eval[(H :++: T)#Apply[B]] =
    ra match {
      case Inl(h) => H.gmap(h)(f).map(Inl(_))
      case Inr(t) => T.gmap(t)(f).map(Inr(_))
    }
}

private[cats] trait GFunctorCNil extends GFunctor[CNil1] {
  def gmap[A, B](ra: CNil)(f: A => Eval[B]): Eval[CNil] = ???
}

private[cats] trait GFunctorConst[K] extends GFunctor[Const1[K]] {
  def gmap[A, B](ra: K)(f: A => Eval[B]): Eval[K] = Eval.now(ra)
}

private[cats] trait GFunctorParam extends GFunctor[Param1] {
  def gmap[A, B](ra: A)(f: A => Eval[B]): Eval[B] = f(ra)
}

private[cats] trait GFunctorRec[F[_]] extends GFunctor[Rec1[F]] {
  val functor: Functor[F]
  def gmap[A, B](ra: F[A])(f: A => Eval[B]): Eval[F[B]] = Eval.later(functor.map(ra)(f(_).value))
}

trait DerivedFunctor[F[_], R <: TypeFunction1] extends Functor[F] {
  val generic1: Generic1.Aux[F, R]
  val gfunctor: GFunctor[R]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    gfunctor.gmap(generic1.embed(fa))(a => Eval.later(f(a))).map(generic1.project(_)).value
}

trait DeriveFunctor {
  implicit def deriveFunctor[F[_], R <: TypeFunction1](implicit FR: Generic1.Aux[F, R], R: GFunctor[R]): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        R.gmap(FR.embed(fa))(a => Eval.later(f(a))).map(FR.project(_)).value
    }
}
