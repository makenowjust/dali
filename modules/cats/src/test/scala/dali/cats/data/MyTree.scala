package dali
package cats
package data

import _root_.cats.kernel.Eq
import _root_.cats.syntax.eq._
import org.scalacheck.{Arbitrary, Gen}

case class MyTree[A](root: A, children: MyList[MyTree[A]])

object MyTree {
  implicit def eq[A: Eq]: Eq[MyTree[A]] = {
    def eqv(x: MyTree[A], y: MyTree[A]): Boolean =
      x.root === y.root &&
        x.children.size == y.children.size &&
        x.children.toList.zip(y.children.toList).forall { case (x1, y1) => eqv(x1, y1) }

    Eq.instance(eqv(_, _))
  }

  implicit def arbitrary[A: Arbitrary]: Arbitrary[MyTree[A]] =
    Arbitrary(for {
      root <- Arbitrary.arbitrary[A]
      size <- Gen.size
      children <- Gen.resize(size / 2, Gen.delay(MyList.arbitrary(arbitrary).arbitrary))
    } yield MyTree(root, children))
}
