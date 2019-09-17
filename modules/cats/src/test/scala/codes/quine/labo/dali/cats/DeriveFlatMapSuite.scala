package codes.quine.labo
package dali
package cats

import data._
import derive.flatMap._

import _root_.cats.instances.list._
import _root_.cats.instances.map._
import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.instances.string._
import _root_.cats.kernel.instances.tuple._
import _root_.cats.laws.discipline.FlatMapTests
import minitest.SimpleTestSuite

object DeriveFlatMapSuite extends SimpleTestSuite with DisciplineCheckers {
  checkAll("FlatMapTests[Pair]", FlatMapTests[Pair].flatMap[Int, Int, String])
  checkAll("FlatMapTests[Lambda[A => (A, Map[Int, A])]]",
           FlatMapTests[Lambda[A => (A, Map[Int, A])]].flatMap[Int, Int, String])
  checkAll("FlatMapTests[Lambda[A => (A, List[A])]", FlatMapTests[Lambda[A => (A, List[A])]].flatMap[Int, Int, String])
}
