package dali
package singleton

import macros._

trait Valuable[A] {
  type Value
  def value: Value
}

object Valuable {
  import scala.language.experimental.macros

  type Aux[A, V] = Valuable[A] { type Value = V }

  def apply[A](implicit A: Valuable[A]): Aux[A, A.Value] = A
  def eval[A](implicit A: Valuable[A]): A.Value = A.value

  implicit def evaluate[E, V]: Aux[E, V] = macro ValuableMacros.evaluate[E]
}

