package dali
package cats

import data._
import derive.monad._

import _root_.cats.instances.list._
import _root_.cats.kernel.instances.int._
import _root_.cats.kernel.instances.string._
import _root_.cats.kernel.instances.tuple._
import _root_.cats.laws.discipline.MonadTests
import minitest.SimpleTestSuite

object DeriveMonadSuite extends SimpleTestSuite with DisciplineCheckers {
  checkAll("MonadTests[Pair]", MonadTests[Pair].monad[Int, Int, String])
  checkAll("MonadTests[Lambda[A => (A, List[A])]]", MonadTests[Lambda[A => (A, List[A])]].monad[Int, Int, String])
}
