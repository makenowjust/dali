package dali
package cats

import minitest.SimpleTestSuite
import minitest.laws.Checkers
import org.scalacheck.Test.Parameters
import org.typelevel.discipline.Laws

trait DisciplineCheckers extends Checkers { self: SimpleTestSuite =>
  def checkAll(name: String, ruleSet: Laws#RuleSet): Unit =
    for ((id, prop) <- ruleSet.all.properties) {
      self.test(s"$name.$id") {
        check(prop, Parameters.default.withMaxSize(10).withWorkers(2))
      }
    }
}
