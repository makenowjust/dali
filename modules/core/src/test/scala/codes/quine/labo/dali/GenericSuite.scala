package codes.quine.labo
package dali

import data._

import minitest.SimpleTestSuite

object GenericSuite extends SimpleTestSuite {
  def assertGeneric[A: Generic](a: A): Unit =
    assertEquals(Generic[A].project(Generic[A].embed(a)), a)

  test("Generic: Expr") {
    val v1 = Val(1)
    val v2 = Val(2)
    val add = Add(v1, v2)

    Generic[Expr]: Generic.Aux[Expr, Add :+: Val :+: Var.type :+: CNil]
    Generic[Var.type]: Generic.Aux[Var.type, HNil]
    Generic[Val]: Generic.Aux[Val, Int :*: HNil]
    Generic[Add]: Generic.Aux[Add, Expr :*: Expr :*: HNil]

    assertGeneric[Expr](Var)
    assertGeneric[Expr](v1)
    assertGeneric[Expr](v2)
    assertGeneric[Expr](add)
    assertGeneric(Var)
    assertGeneric(v1)
    assertGeneric(v2)
    assertGeneric(add)
  }

  test("Generic: MyList") {
    val nil = MyNil[Int]
    val cons = MyCons(1, nil)
    val listCons = MyCons(List(1, 2), MyNil[List[Int]])

    Generic[MyList[Int]]: Generic.Aux[MyList[Int], MyCons[Int] :+: MyNil[Int] :+: CNil]
    Generic[MyNil[Int]]: Generic.Aux[MyNil[Int], HNil]
    Generic[MyCons[Int]]: Generic.Aux[MyCons[Int], Int :*: MyList[Int] :*: HNil]

    assertGeneric[MyList[Int]](nil)
    assertGeneric[MyList[Int]](cons)
    assertGeneric(nil)
    assertGeneric(cons)

    assertGeneric[MyList[List[Int]]](listCons)
    assertGeneric(listCons)
  }

  test("Generic: Unit, Tuple2, Tuple3") {
    assertGeneric(())
    assertGeneric((1, "foo"))
    assertGeneric((1, "foo", 3))
  }

  test("Generic: (Int, Map[Int, List[Int]])") {
    type Foo = (Int, Map[Int, List[Int]])
    Generic[Foo]: Generic.Aux[Foo, Int :*: Map[Int, List[Int]] :*: HNil]
    assertGeneric((1, Map(2 -> List(3))))
  }
}
