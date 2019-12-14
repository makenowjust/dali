package dali
package singleton

import op.HListAt._

import minitest.SimpleTestSuite

object ValuableSuite extends SimpleTestSuite {
  test("Valuable: binary") {
    def assertBinary[
      X, Y,
      Add: ValueOf: Valuable.Aux[X |+| Y, *],
      Sub: ValueOf: Valuable.Aux[X |-| Y, *],
      Mul: ValueOf: Valuable.Aux[X |*| Y, *],
      Div: ValueOf: Valuable.Aux[X |/| Y, *],
      Mod: ValueOf: Valuable.Aux[X |%| Y, *]
    ]: Unit = {
      assertEquals(Valuable[X |+| Y].value, valueOf[Add])
      assertEquals(Valuable[X |-| Y].value, valueOf[Sub])
      assertEquals(Valuable[X |*| Y].value, valueOf[Mul])
      assertEquals(Valuable[X |/| Y].value, valueOf[Div])
      assertEquals(Valuable[X |%| Y].value, valueOf[Mod])
    }

    assertBinary[4, 3, 7, 1, 12, 1, 1]
    assertBinary[4L, 3L, 7L, 1L, 12L, 1L, 1L]
    assertBinary[4.0F, 2.0F, 6.0F, 2.0F, 8.0F, 2.0F, 0.0F]
    assertBinary[4.0, 2.0, 6.0, 2.0, 8.0, 2.0, 0.0]
  }

  test("Valuable: HListAt") {
    val xs: Int :*: String :*: Double :*: HNil = 1 :*: "foo" :*: 4.2 :*: HNil
    assertEquals(xs.at(0), 1)
    assertEquals(xs.at(1), "foo")
    assertEquals(xs.at(2), 4.2)
  }
}
