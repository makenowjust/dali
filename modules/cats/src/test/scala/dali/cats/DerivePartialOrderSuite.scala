package dali
package cats

import data._
import derive.partialOrder._

import _root_.cats.kernel.PartialOrder
import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.laws.discipline.PartialOrderTests
import minitest.SimpleTestSuite

object DerivePartialOrderSuite extends SimpleTestSuite with DisciplineCheckers {
  implicit def partialOrderList[A: PartialOrder]: PartialOrder[MyList[A]] = derivePartialOrder
  implicit def partialOrderTree[A: PartialOrder]: PartialOrder[MyTree[A]] = derivePartialOrder

  checkAll("PartialOrderTests[Either[Int, Int]]", PartialOrderTests[Either[Int, Int]].eqv)
  checkAll("PartialOrderTests[(Int, Int)]", PartialOrderTests[(Int, Int)].eqv)

  checkAll("PartialOrderTests[MyList[Int]]", PartialOrderTests[MyList[Int]].eqv)
  checkAll("PartialOrderTests[MyTree[Int]]", PartialOrderTests[MyTree[Int]].eqv)
}
