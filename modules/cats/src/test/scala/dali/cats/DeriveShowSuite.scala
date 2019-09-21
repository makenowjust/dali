package dali
package cats

import data._
import derive.show._

import _root_.cats.Show
import _root_.cats.instances.int._
import minitest.SimpleTestSuite

object DeriveShowSuite extends SimpleTestSuite {
  implicit def showList[A: Show]: Show[MyList[A]] = deriveShowForCoproduct
  implicit def showTree[A: Show]: Show[MyTree[A]] = deriveShowForProduct

  test("DeriveShow: MyList") {
    assertEquals(
      Show[MyList[Int]].show(MyList(1, 2, 3)),
      "MyCons(1, MyCons(2, MyCons(3, MyNil)))"
    )
  }

  test("DeriveShow: MyTree") {
    assertEquals(
      Show[MyTree[Int]].show(MyTree(1, MyList(MyTree(2, MyList[MyTree[Int]]())))),
      "MyTree(1, MyCons(MyTree(2, MyNil), MyNil))"
    )
  }

  test("DeriveShow: Unit") {
    assertEquals(Show[Unit].show(()), "()")
  }

  test("DeriveShow: (Int, Int)") {
    assertEquals(Show[(Int, Int)].show((1, 2)), "(1, 2)")
    assertEquals(Show[Pair[Int]].show((1, 2)), "(1, 2)")
  }
}
