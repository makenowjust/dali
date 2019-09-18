package dali

import data._

import minitest.SimpleTestSuite

object LabelledGenericSuite extends SimpleTestSuite {
  val LG = LabelledGeneric
  type L[L0 <: String with Singleton, A] = Labelled[L0, A]

  def assertLabelledGeneric[A: LabelledGeneric](a: A): Unit =
    assertEquals(LG[A].project(LG[A].embed(a)), a)

  test("LabelledGeneric: Expr") {
    val v1 = Val(1)
    val v2 = Val(2)
    val add = Add(v1, v2)

    LG[Expr]: LG.Aux[Expr, "Expr", L["Add", Add] :+: L["Val", Val] :+: L["Var", Var.type] :+: CNil]
    LG[Var.type]: LG.Aux[Var.type, "Var", HNil]
    LG[Val]: LG.Aux[Val, "Val", L["x", Int] :*: HNil]
    LG[Add]: LG.Aux[Add, "Add", L["l", Expr] :*: L["r", Expr] :*: HNil]

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

    LG[MyList[Int]]: LG.Aux[MyList[Int], "MyList", L["MyCons", MyCons[Int]] :+: L["MyNil", MyNil[Int]] :+: CNil]
    LG[MyCons[Int]]: LG.Aux[MyCons[Int], "MyCons", L["head", Int] :*: L["tail", MyList[Int]] :*: HNil]
    LG[MyNil[Int]]: LG.Aux[MyNil[Int], "MyNil", HNil]

    assertLabelledGeneric[MyList[Int]](nil)
    assertLabelledGeneric[MyList[Int]](cons)
    assertLabelledGeneric(nil)
    assertLabelledGeneric(cons)

    assertLabelledGeneric[MyList[List[Int]]](listCons)
    assertLabelledGeneric(listCons)
  }
}
