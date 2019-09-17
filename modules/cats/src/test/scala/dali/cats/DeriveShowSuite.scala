package dali
package cats

import data._
import derive.show._

import _root_.cats.Show
import _root_.cats.instances.int._
import minitest.SimpleTestSuite

object DeriveShowSuite extends SimpleTestSuite {
  // NOTE: it is necessary to avoid "diverging implicit expansion" error on Show[MyTree[Int]] derivation.
  //       But I don't know why it is needed really. Is this Scala compiler BUG?
  //       And bonus, it is not needed when there is custom Show[MyList[A]] instance instaed of derivated instance.
  implicit def showList[A: Show]: Show[MyList[A]] = {
    import derive.show._
    Show[MyList[A]]
  }

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
