package dali
package cats
package data

import _root_.cats.kernel.Eq
import org.scalacheck.{Arbitrary, Cogen, Gen}

sealed trait MyList[A] {
  def toList: List[A] =
    this match {
      case MyNil()            => List.empty
      case MyCons(head, tail) => head :: tail.toList
    }
  def size: Int =
    this match {
      case MyNil()         => 0
      case MyCons(_, tail) => 1 + tail.size
    }
}
case class MyNil[A]() extends MyList[A]
case class MyCons[A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def apply[A](vs: A*): MyList[A] = fromList(vs.toList)

  def fromList[A](l: List[A]): MyList[A] =
    l.foldRight(MyNil[A]: MyList[A])(MyCons(_, _))

  implicit def eq[A: Eq]: Eq[MyList[A]] = {
    import derive.eq._
    deriveEq
  }

  implicit def arbitrary[A](implicit A: => Arbitrary[A]): Arbitrary[MyList[A]] =
    Arbitrary(Gen.listOf(A.arbitrary).map(fromList(_)))

  implicit def cogen[A](implicit A: => Cogen[A]): Cogen[MyList[A]] =
    Cogen[MyList[A]]((seed, t) => Cogen.cogenList(A).contramap((_: MyList[A]).toList).perturb(seed, t))
}
