package dali
package higher

import data._

import minitest.SimpleTestSuite

object Generic1Suite extends SimpleTestSuite {
  def assertGeneric1[F[_]: Generic1, A](fa: F[A]): Unit =
    assertEquals(Generic1[F].project(Generic1[F].embed(fa)), fa)

  test("Generic1: MyList") {
    val nil = MyNil[Int]
    val cons = MyCons(1, nil)

    Generic1[MyList]: Generic1.Aux[MyList, Rec1[MyCons] :++: Rec1[MyNil] :++: CNil1]
    Generic1[MyNil]: Generic1.Aux[MyNil, HNil1]
    Generic1[MyCons]: Generic1.Aux[MyCons, Param1 :**: Rec1[MyList] :**: HNil1]

    assertGeneric1[MyList, Int](nil)
    assertGeneric1[MyList, Int](cons)
    assertGeneric1(nil)
    assertGeneric1(cons)

    assertEquals(Functor[MyList].map(cons)(_ + 1), MyCons(2, MyNil[Int]))
  }

  test("Generic1: MyEither") {
    val left = MyLeft[String, Int]("foo")
    val right = MyRight[String, Int](1)

    type MyEitherRepr = Rec1[MyLeft[String, *]] :++: Rec1[MyRight[String, *]] :++: CNil1
    Generic1[MyEither[String, *]]: Generic1.Aux[MyEither[String, *], MyEitherRepr]
    Generic1[MyLeft[String, *]]: Generic1.Aux[MyLeft[String, *], Const1[String] :**: HNil1]
    Generic1[MyRight[String, *]]: Generic1.Aux[MyRight[String, *], Param1 :**: HNil1]

    assertGeneric1[MyEither[String, *], Int](left)
    assertGeneric1[MyEither[String, *], Int](right)
    assertGeneric1(left)
    assertGeneric1(right)

    assertEquals(Functor[MyEither[String, *]].map(left)(_ + 1), left)
    assertEquals(Functor[MyEither[String, *]].map(right)(_ + 1), MyRight(2))
  }

  test("Generic1: scala.util.Either") {
    val left = Left[String, Int]("foo")
    val right = Right[String, Int](1)

    Generic1[Either[String, *]]: Generic1.Aux[Either[String, *], Rec1[Left[String, *]] :++: Rec1[Right[String, *]] :++: CNil1]
    Generic1[Left[String, *]]: Generic1.Aux[Left[String, *], Const1[String] :**: HNil1]
    Generic1[Right[String, *]]: Generic1.Aux[Right[String, *], Param1 :**: HNil1]

    assertGeneric1[Either[String, *], Int](left)
    assertGeneric1[Either[String, *], Int](right)
    assertGeneric1(left)
    assertGeneric1(right)

    assertEquals(Functor[Either[String, *]].map(left)(_ + 1), left)
    assertEquals(Functor[Either[String, *]].map(right)(_ + 1), Right(2))
  }

  test("Generic1: Lambda[A => (A, Map[Int, A])]") {
    type Foo[A] = (A, Map[Int, A])

    Generic1[Foo]: Generic1.Aux[Foo, Param1 :**: Rec1[Map[Int, *]] :**: HNil1]

    assertGeneric1[Foo, Int]((1, Map(1 -> 2)))
  }
}
