package codes.quine.labo
package dali
package cats

import data._
import derive.apply._

import _root_.cats.instances.list._
import _root_.cats.instances.map._
import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.instances.string._
import _root_.cats.kernel.instances.tuple._
import _root_.cats.laws.discipline.ApplyTests
import minitest.SimpleTestSuite

object DeriveApplySuite extends SimpleTestSuite with DisciplineCheckers {
  checkAll("ApplyTests[Pair]", ApplyTests[Pair].apply[Int, Int, String])
  checkAll("ApplyTests[Lambda[A => (String, A, Map[Int, A])]]",
           ApplyTests[Lambda[A => (String, A, Map[Int, A])]].apply[Int, Int, String])
  checkAll("ApplyTests[Lambda[A => (String, A, List[A])]]",
           ApplyTests[Lambda[A => (String, A, List[A])]].apply[Int, Int, String])
}
