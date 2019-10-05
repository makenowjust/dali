package dali
package cats

import data._, derive.eq._
import _root_.cats.kernel.Eq
import org.scalacheck.{Arbitrary, Cogen, Gen}

package object instances {
  implicit def listEq[A: Eq]: Eq[MyList[A]] = deriveEq

  implicit def listArbitrary[A](implicit A: => Arbitrary[A]): Arbitrary[MyList[A]] =
    Arbitrary(Gen.listOf(A.arbitrary).map(MyList.fromList(_)))

  implicit def listCogen[A](implicit A: => Cogen[A]): Cogen[MyList[A]] =
    Cogen[MyList[A]]((seed, t) => Cogen.cogenList(A).contramap((_: MyList[A]).toList).perturb(seed, t))

  implicit def treeEq[A: Eq]: Eq[MyTree[A]] = deriveEq

  implicit def treeArbitrary[A: Arbitrary]: Arbitrary[MyTree[A]] =
    Arbitrary(for {
      root <- Arbitrary.arbitrary[A]
      size <- Gen.size
      children <- Gen.resize(size / 2, Gen.delay(listArbitrary(treeArbitrary).arbitrary))
    } yield MyTree(root, children))

  implicit def treeCogen[A: Cogen]: Cogen[MyTree[A]] =
    Cogen[(A, MyList[MyTree[A]])].contramap { case MyTree(r, cs) => (r, cs) }
}
