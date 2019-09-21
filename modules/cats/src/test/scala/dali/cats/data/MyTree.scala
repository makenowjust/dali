package dali
package cats
package data

import _root_.cats.kernel.Eq
import org.scalacheck.{Arbitrary, Cogen, Gen}

case class MyTree[A](root: A, children: MyList[MyTree[A]])

object MyTree {
  implicit def eq[A: Eq]: Eq[MyTree[A]] = {
    import derive.eq._
    deriveEq
  }

  implicit def arbitrary[A: Arbitrary]: Arbitrary[MyTree[A]] =
    Arbitrary(for {
      root <- Arbitrary.arbitrary[A]
      size <- Gen.size
      children <- Gen.resize(size / 2, Gen.delay(MyList.arbitrary(arbitrary).arbitrary))
    } yield MyTree(root, children))

  implicit def cogen[A: Cogen]: Cogen[MyTree[A]] =
    Cogen[(A, MyList[MyTree[A]])].contramap { case MyTree(r, cs) => (r, cs) }
}
