package codes.quine.labo
package dali

import minitest.SimpleTestSuite

object HListSuite extends SimpleTestSuite {
  test("HList: construction") {
    assert(HNil.isInstanceOf[HNil])
    assert((1 :*: HNil).isInstanceOf[Int :*: HNil])
  }

  test("HList: ==") {
    assert(HNil == HNil)
    assert((1 :*: HNil) == (1 :*: HNil))
  }

  test("HList: toString") {
    assertEquals(HNil.toString, "HNil")
    assertEquals((1 :*: HNil).toString, "1 :*: HNil")
  }

  test("HList: pattern matching") {
    assert(HNil match { case HNil => true })

    assert((1 :*: HNil) match { case i :*: HNil => i == 1 })
  }
}
