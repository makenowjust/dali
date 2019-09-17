package dali
package cats

import data._
import derive.applicative._

import _root_.cats.instances.list._
import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.instances.string._
import _root_.cats.kernel.instances.tuple._
import _root_.cats.laws.discipline.ApplicativeTests
import minitest.SimpleTestSuite

object DeriveApplicativeSuite extends SimpleTestSuite with DisciplineCheckers {
  checkAll("ApplicativeTests[Pair]", ApplicativeTests[Pair].applicative[Int, Int, String])
  checkAll("ApplicativeTests[Lambda[A => (String, A, List[A])]]",
           ApplicativeTests[Lambda[A => (String, A, List[A])]].applicative[Int, Int, String])
}
