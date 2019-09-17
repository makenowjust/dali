package codes.quine.labo
package dali
package cats

import data._
import derive.traverse._

import _root_.cats.kernel.instances.double._
import _root_.cats.kernel.instances.either._
import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.instances.long._
import _root_.cats.kernel.instances.string._
import _root_.cats.kernel.instances.tuple._
import _root_.cats.instances.option._
import _root_.cats.laws.discipline.TraverseTests
import minitest.SimpleTestSuite

object DeriveTraverseSuite extends SimpleTestSuite with DisciplineCheckers {
  checkAll("TraverseTests[Either[Int, *]]",
           TraverseTests[Either[Int, *]].traverse[Int, Double, String, Long, Option, Option])
  checkAll("TraverseTests[Either[*, Int]]",
           TraverseTests[Either[*, Int]].traverse[Int, Double, String, Long, Option, Option])
  checkAll("TraverseTests[(Int, *)]", TraverseTests[(Int, *)].traverse[Int, Double, String, Long, Option, Option])
  checkAll("TraverseTests[(*, Int)]", TraverseTests[(*, Int)].traverse[Int, Double, String, Long, Option, Option])

  checkAll("TraverseTests[Pair]", TraverseTests[Pair].traverse[Int, Double, String, Long, Option, Option])
  checkAll("TraverseTests[MyList]", TraverseTests[MyList].traverse[Int, Double, String, Long, Option, Option])
  checkAll("TraverseTests[MyTree]", TraverseTests[MyTree].traverse[Int, Double, String, Long, Option, Option])
}
