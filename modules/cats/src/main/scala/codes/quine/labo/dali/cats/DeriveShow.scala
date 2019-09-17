package codes.quine.labo
package dali
package cats

import _root_.cats.Show

trait GShow[R] {
  def gshow(r: R): String
}

object GShow {
  def apply[R: GShow]: GShow[R] = implicitly

  implicit def hcons[H: GShow, T <: HList: GShow]: GShow[H :*: T] =
    new GShow[H :*: T] {
      def gshow(r: H :*: T): String = {
        val h = GShow[H].gshow(r.head)
        val t = GShow[T].gshow(r.tail)
        if (t.isEmpty) h else s"$h, $t"
      }
    }

  implicit def hnil: GShow[HNil] = new GShow[HNil] {
    def gshow(r: HNil): String = ""
  }

  implicit def ccons[H: GShow, T <: Coproduct: GShow]: GShow[H :+: T] =
    new GShow[H :+: T] {
      def gshow(r: H :+: T): String =
        r match {
          case Inl(h) => GShow[H].gshow(h)
          case Inr(t) => GShow[T].gshow(t)
        }
    }

  implicit def cnil: GShow[CNil] = new GShow[CNil] {
    def gshow(r: CNil): String = ???
  }

  implicit def field[A](implicit A: => Show[A]): GShow[A] = new GShow[A] {
    def gshow(r: A): String = A.show(r)
  }

  implicit def labelled[L <: String with Singleton, A](implicit A: => Show[A]): GShow[Labelled[L, A]] =
    new GShow[Labelled[L, A]] {
      def gshow(r: Labelled[L, A]): String = A.show(r.value)
    }
}

trait DeriveShow extends DeriveShowLowPriority {
  implicit def deriveShowForCoproduct[A, R <: Coproduct](implicit AR: Generic.Aux[A, R], R: GShow[R]): Show[A] =
    new Show[A] {
      def show(a: A): String = R.gshow(AR.embed(a))
    }
}

private[cats] trait DeriveShowLowPriority {
  implicit def deriveShowForProduct[A, L <: String with Singleton, R <: HList](
    implicit ARL: LabelledGeneric.Aux[A, L, R],
    L: ValueOf[L],
    R: GShow[R]
  ): Show[A] =
    new Show[A] {
      def show(a: A): String = {
        val prefix = ARL.label match {
          case s"Tuple$_" => ""
          case "Unit"     => ""
          case l          => l
        }
        R.gshow(ARL.embed(a)) match {
          case "" if prefix != "" => prefix
          case v                  => s"$prefix($v)"
        }
      }
    }
}
