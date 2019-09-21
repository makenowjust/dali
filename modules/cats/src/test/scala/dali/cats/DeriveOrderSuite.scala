package dali
package cats

import derive.order._

import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.instances.option._
import _root_.cats.kernel.instances.string._
import _root_.cats.kernel.laws.discipline.OrderTests
import minitest.SimpleTestSuite

object DeriveOrderSuite extends SimpleTestSuite with DisciplineCheckers {
  checkAll("OrderTests[(Int, Int)]", OrderTests[(Int, Int)].order)
  checkAll("OrderTests[(Int, String)]", OrderTests[(Int, String)].order)
}
