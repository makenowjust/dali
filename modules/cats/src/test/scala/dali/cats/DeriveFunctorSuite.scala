package dali
package cats

import data._, instances._
import derive.functor._

import _root_.cats.kernel.instances.either._
import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.instances.string._
import _root_.cats.kernel.instances.tuple._
import _root_.cats.laws.discipline.FunctorTests
import minitest.SimpleTestSuite

object DeriveFunctorSuite extends SimpleTestSuite with DisciplineCheckers {
  checkAll("FunctorTests[Either[String, *]]", FunctorTests[Either[String, *]].functor[Int, Int, String])
  checkAll("FunctorTests[Either[*, String]]", FunctorTests[Either[*, String]].functor[Int, Int, String])
  checkAll("FunctorTests[(String, *)]", FunctorTests[(String, *)].functor[Int, Int, String])
  checkAll("FunctorTests[(*, String)]", FunctorTests[(*, String)].functor[Int, Int, String])

  checkAll("FunctorTests[Pair]", FunctorTests[Pair].functor[Int, Int, String])
  checkAll("FunctorTests[MyList]", FunctorTests[MyList].functor[Int, Int, String])
  checkAll("FunctorTests[MyTree]", FunctorTests[MyTree].functor[Int, Int, String])
}
