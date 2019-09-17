package codes.quine.labo
package dali

import data._

import minitest.SimpleTestSuite

object LabelledGenericSuite extends SimpleTestSuite {
  def assertLabelledGeneric[A: LabelledGeneric](a: A): Unit =
    assertEquals(LabelledGeneric[A].project(LabelledGeneric[A].embed(a)), a)

  test("LabelledGeneric: Expr") {
    val v1 = Val(1)
    val v2 = Val(2)
    val add = Add(v1, v2)

    LabelledGeneric[Expr]: LabelledGeneric.Aux[Expr, "Expr", Labelled["Add", Add] :+: Labelled["Val", Val] :+: Labelled[
      "Var",
      Var.type
    ] :+: CNil]
    LabelledGeneric[Var.type]: LabelledGeneric.Aux[Var.type, "Var", HNil]
    LabelledGeneric[Val]: LabelledGeneric.Aux[Val, "Val", Labelled["x", Int] :*: HNil]
    LabelledGeneric[Add]: LabelledGeneric.Aux[Add, "Add", Labelled["l", Expr] :*: Labelled["r", Expr] :*: HNil]

    assertLabelledGeneric[Expr](Var)
    assertLabelledGeneric[Expr](v1)
    assertLabelledGeneric[Expr](v2)
    assertLabelledGeneric[Expr](add)
    assertLabelledGeneric(Var)
    assertLabelledGeneric(v1)
    assertLabelledGeneric(v2)
    assertLabelledGeneric(add)
  }

  test("LabelledGeneric: MyList") {
    val nil = MyNil[Int]
    val cons = MyCons(1, nil)
    val listCons = MyCons(List(1, 2), MyNil[List[Int]])

    LabelledGeneric[MyList[Int]]: LabelledGeneric.Aux[MyList[Int], "MyList", Labelled["MyCons", MyCons[Int]] :+: Labelled[
      "MyNil",
      MyNil[Int]
    ] :+: CNil]
    LabelledGeneric[MyCons[Int]]: LabelledGeneric.Aux[MyCons[Int], "MyCons", Labelled["head", Int] :*: Labelled[
      "tail",
      MyList[
        Int
      ]
    ] :*: HNil]
    LabelledGeneric[MyNil[Int]]: LabelledGeneric.Aux[MyNil[Int], "MyNil", HNil]

    assertLabelledGeneric[MyList[Int]](nil)
    assertLabelledGeneric[MyList[Int]](cons)
    assertLabelledGeneric(nil)
    assertLabelledGeneric(cons)

    assertLabelledGeneric[MyList[List[Int]]](listCons)
    assertLabelledGeneric(listCons)
  }
}
