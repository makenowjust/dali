package dali
package higher

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(a: A => B): F[B]
}

object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly

  implicit def gfunctor[F[_], R <: TypeFunction1](implicit FR: Generic1.Aux[F, R], R: GFunctor[R]): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] = FR.project(R.gmap(FR.embed(fa))(f))
    }
}

trait GFunctor[R <: TypeFunction1] {
  def gmap[A, B](ra: R#Apply[A])(f: A => B): R#Apply[B]
}

object GFunctor {
  def apply[R <: TypeFunction1: GFunctor]: GFunctor[R] = implicitly

  implicit def hcons[H <: TypeFunction1, T <: HList1](implicit H: GFunctor[H], T: GFunctor[T]): GFunctor[H :**: T] =
    new GFunctor[H :**: T] {
      def gmap[A, B](ra: (H :**: T)#Apply[A])(f: A => B): (H :**: T)#Apply[B] =
        H.gmap(ra.head)(f) :*: T.gmap(ra.tail)(f)
    }

  implicit val hnil: GFunctor[HNil1] = new GFunctor[HNil1] {
    def gmap[A, B](ra: HNil)(f: A => B): HNil = HNil
  }

  implicit def ccons[H <: TypeFunction1, T <: Coproduct1](implicit H: GFunctor[H], T: GFunctor[T]): GFunctor[H :++: T] =
    new GFunctor[H :++: T] {
      def gmap[A, B](ra: (H :++: T)#Apply[A])(f: A => B): (H :++: T)#Apply[B] =
        ra match {
          case Inl(h) => Inl(H.gmap(h)(f))
          case Inr(t) => Inr(T.gmap(t)(f))
        }
    }

  implicit val cnil: GFunctor[CNil1] = new GFunctor[CNil1] {
    def gmap[A, B](ra: CNil)(f: A => B): CNil = ???
  }

  implicit def const[A]: GFunctor[Const1[A]] = new GFunctor[Const1[A]] {
    def gmap[B, C](ra: A)(f: B => C): A = ra
  }

  implicit val param: GFunctor[Param1] = new GFunctor[Param1] {
    def gmap[A, B](ra: A)(f: A => B): B = f(ra)
  }

  implicit def rec[F[_]](implicit F: => Functor[F]): GFunctor[Rec1[F]] = new GFunctor[Rec1[F]] {
    def gmap[A, B](ra: F[A])(f: A => B): F[B] = F.map(ra)(f)
  }
}
