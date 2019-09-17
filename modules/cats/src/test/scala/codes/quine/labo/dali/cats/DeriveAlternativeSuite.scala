package codes.quine.labo
package dali
package cats

import derive.alternative._

import _root_.cats.instances.list._
import _root_.cats.instances.option._
import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.instances.string._
import _root_.cats.kernel.instances.tuple._
import _root_.cats.laws.discipline.AlternativeTests
import minitest.SimpleTestSuite

object DeriveAlternativeSuite extends SimpleTestSuite with DisciplineCheckers {
  checkAll(
    "AlternativeTests[Lambda[A => (List[A], Option[A])]]",
    AlternativeTests[Lambda[A => (List[A], Option[A])]].alternative[Int, Int, String]
  )
}
