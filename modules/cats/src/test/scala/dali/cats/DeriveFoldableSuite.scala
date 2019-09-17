package dali
package cats

import data._
import derive.foldable._

import _root_.cats.kernel.instances.either._
import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.instances.option._
import _root_.cats.kernel.instances.tuple._
import _root_.cats.laws.discipline.FoldableTests
import minitest.SimpleTestSuite

object DeriveFodableSuite extends SimpleTestSuite with DisciplineCheckers {
  checkAll("FoldableTests[Either[Int, *]]", FoldableTests[Either[Int, *]].foldable[Int, Int])
  checkAll("FoldableTests[Either[*, Int]]", FoldableTests[Either[*, Int]].foldable[Int, Int])
  checkAll("FoldableTests[(Int, *))]", FoldableTests[(Int, *)].foldable[Int, Int])
  checkAll("FoldableTests[(*, Int))]", FoldableTests[(*, Int)].foldable[Int, Int])

  checkAll("FoldableTests[Pair]", FoldableTests[Pair].foldable[Int, Int])
  checkAll("FoldableTests[MyList]", FoldableTests[MyList].foldable[Int, Int])
  checkAll("FoldableTests[MyTree]", FoldableTests[MyTree].foldable[Int, Int])
}
