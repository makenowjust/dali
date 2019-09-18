package dali

import macros._

trait LabelledGeneric[A] {
  type Label <: String with Singleton

  // NOTE: these methods are the same as Generic. However LabelledGeneric is not inherited from Generic,
  //       because it is not good that passing LabelledGeneric to a method needs Generic.
  type Repr
  def embed(a: A): Repr
  def project(r: Repr): A
}

object LabelledGeneric {
  import scala.language.experimental.macros

  type Aux[A, L <: String with Singleton, R] = LabelledGeneric[A] {
    type Label = L
    type Repr = R
  }

  def apply[A](implicit lg: LabelledGeneric[A]): Aux[A, lg.Label, lg.Repr] = lg

  implicit def materialize[A, L <: String with Singleton, R]: Aux[A, L, R] =
    macro GenericMacros.materializeLabelled[A, L, R]

  implicit val unit: LabelledGeneric.Aux[Unit, "Unit", HNil] = new LabelledGeneric[Unit] {
    type Repr = HNil
    def embed(a: Unit): Repr = HNil
    def project(r: Repr): Unit = ()
    type Label = "Unit"
  }
}
