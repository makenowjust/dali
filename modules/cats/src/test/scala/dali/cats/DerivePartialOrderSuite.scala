package dali
package cats

import data._, instances._
import derive.partialOrder._

import _root_.cats.kernel.PartialOrder
import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.instances.option._
import _root_.cats.kernel.laws.discipline.PartialOrderTests
import minitest.SimpleTestSuite

object DerivePartialOrderSuite extends SimpleTestSuite with DisciplineCheckers {
  implicit def listPartialOrder[A: PartialOrder]: PartialOrder[MyList[A]] = derivePartialOrder
  implicit def treePartialOrder[A: PartialOrder]: PartialOrder[MyTree[A]] = derivePartialOrder

  checkAll("PartialOrderTests[Either[Int, Int]]", PartialOrderTests[Either[Int, Int]].partialOrder)
  checkAll("PartialOrderTests[(Int, Int)]", PartialOrderTests[(Int, Int)].partialOrder)

  checkAll("PartialOrderTests[MyList[Int]]", PartialOrderTests[MyList[Int]].partialOrder)
  checkAll("PartialOrderTests[MyTree[Int]]", PartialOrderTests[MyTree[Int]].partialOrder)
}
