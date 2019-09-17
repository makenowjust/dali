package dali
package cats

import higher._

import _root_.cats.{Apply, Eval, FlatMap}

trait GFlatMap[R <: TypeFunction1] extends GApply[R] {
  def gflatMap[A, B](ra: R#Apply[A])(f: A => R#Apply[B]): R#Apply[B]
  def gtailRecM[A, B](a: A)(f: A => R#Apply[Either[A, B]]): Eval[R#Apply[B]]
}

object GFlatMap {
  @inline def apply[R <: TypeFunction1: GFlatMap]: GFlatMap[R] = implicitly

  implicit def hcons[H <: TypeFunction1: GFlatMap, T <: HList1: GFlatMap]: GFlatMap[H :**: T] =
    new GFlatMapHCons[H, T] {
      val H = GFlatMap[H]
      val T = GFlatMap[T]
    }

  implicit val hnil: GFlatMap[HNil1] = new GFlatMapHNil {}

  implicit val param: GFlatMap[Param1] = new GFlatMapParam {}

  implicit def rec[F[_]](implicit F: => FlatMap[F]): GFlatMap[Rec1[F]] =
    new GFlatMapRec[F] {
      lazy val flatMap = F
    }
}

private[cats] trait GFlatMapHCons[H <: TypeFunction1, T <: HList1] extends GFlatMap[H :**: T] with GApplyHCons[H, T] {
  val H: GFlatMap[H]
  val T: GFlatMap[T]

  def gflatMap[A, B](ra: (H :**: T)#Apply[A])(f: A => (H :**: T)#Apply[B]): (H :**: T)#Apply[B] =
    ra match {
      case h :*: t => H.gflatMap(h)(f(_).head) :*: T.gflatMap(t)(f(_).tail)
    }

  def gtailRecM[A, B](a: A)(f: A => (H :**: T)#Apply[Either[A, B]]): Eval[(H :**: T)#Apply[B]] =
    for {
      h <- H.gtailRecM(a)(f(_).head)
      t <- T.gtailRecM(a)(f(_).tail)
    } yield h :*: t
}

private[cats] trait GFlatMapHNil extends GFlatMap[HNil1] with GApplyHNil {
  def gflatMap[A, B](ra: HNil)(f: A => HNil): HNil = HNil
  def gtailRecM[A, B](a: A)(f: A => HNil): Eval[HNil] = Eval.now(HNil)
}

private[cats] trait GFlatMapParam extends GFlatMap[Param1] with GApplyParam {
  def gflatMap[A, B](ra: A)(f: A => B): B = f(ra)

  def gtailRecM[A, B](a: A)(f: A => Either[A, B]): Eval[B] =
    FlatMap[Eval].tailRecM(a)(a1 => Eval.now(f(a1)))
}

private[cats] trait GFlatMapRec[F[_]] extends GFlatMap[Rec1[F]] with GApplyRec[F] {
  val flatMap: FlatMap[F]
  override lazy val apply: Apply[F] = flatMap

  def gflatMap[A, B](ra: F[A])(f: A => F[B]): F[B] = flatMap.flatMap(ra)(f)
  def gtailRecM[A, B](a: A)(f: A => F[Either[A, B]]): Eval[F[B]] = Eval.later(flatMap.tailRecM(a)(f))
}

trait DerivedFlatMap[F[_], R <: TypeFunction1] extends FlatMap[F] with DerivedApply[F, R] {
  val gflatMap: GFlatMap[R]
  override lazy val gapply: GApply[R] = gflatMap

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    generic1.project(gflatMap.gflatMap(generic1.embed(fa))(a => generic1.embed(f(a))))

  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
    gflatMap.gtailRecM(a)(a1 => generic1.embed(f(a1))).map(generic1.project(_)).value
}

trait DeriveFlatMap {
  implicit def deriveFlatMap[F[_], R <: TypeFunction1](implicit FR: Generic1.Aux[F, R], R: GFlatMap[R]): FlatMap[F] =
    new DerivedFlatMap[F, R] {
      val generic1 = FR
      val gflatMap = R
    }
}
