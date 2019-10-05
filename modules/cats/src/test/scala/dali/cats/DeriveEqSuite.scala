package dali
package cats

import data._, instances._
import derive.eq._

import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.laws.discipline.EqTests
import minitest.SimpleTestSuite

object DeriveEqSuite extends SimpleTestSuite with DisciplineCheckers {
  checkAll("EqTests[Either[Int, Int]]", EqTests[Either[Int, Int]].eqv)
  checkAll("EqTests[(Int, Int)]", EqTests[(Int, Int)].eqv)

  checkAll("EqTests[MyList[Int]]", EqTests[MyList[Int]].eqv)
  checkAll("EqTests[MyTree[Int]]", EqTests[MyTree[Int]].eqv)
}
