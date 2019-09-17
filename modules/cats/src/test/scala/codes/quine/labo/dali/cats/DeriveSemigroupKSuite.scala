package codes.quine.labo
package dali
package cats

import derive.semigroupK._

import _root_.cats.instances.list._
import _root_.cats.instances.tuple._
import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.instances.string._
import _root_.cats.laws.discipline.SemigroupKTests
import minitest.SimpleTestSuite

object DeriveSemigroupKSuite extends SimpleTestSuite with DisciplineCheckers {
  checkAll(
    "SemigroupKTests[Lambda[A => (String, List[A])]]",
    SemigroupKTests[Lambda[A => (String, List[A])]].semigroupK[Int]
  )
}
