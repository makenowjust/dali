package dali
package cats

import derive.monoidK._

import _root_.cats.instances.list._
import _root_.cats.instances.tuple._
import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.instances.string._
import _root_.cats.laws.discipline.MonoidKTests
import minitest.SimpleTestSuite

object DeriveMonoidKSuite extends SimpleTestSuite with DisciplineCheckers {
  checkAll(
    "MonoidKTests[Lambda[A => (String, List[A])]]",
    MonoidKTests[Lambda[A => (String, List[A])]].monoidK[Int]
  )
}
