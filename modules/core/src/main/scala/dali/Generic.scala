package dali

import macros._

trait Generic[A] {
  type Repr
  def embed(a: A): Repr
  def project(r: Repr): A
}

object Generic {
  import scala.language.experimental.macros

  type Aux[A, R] = Generic[A] { type Repr = R }

  def apply[A](implicit g: Generic[A]): Aux[A, g.Repr] = g

  implicit def materialize[A, R]: Aux[A, R] = macro GenericMacros.materialize[A, R]

  implicit val unit: Generic.Aux[Unit, HNil] = new Generic[Unit] {
    type Repr = HNil
    def embed(a: Unit): Repr = HNil
    def project(r: Repr): Unit = ()
  }
}
