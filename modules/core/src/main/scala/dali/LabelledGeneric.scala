package dali

import macros._

final class Labelled[L <: String with Singleton, A](val value: A) extends AnyVal {
  @inline final def label(implicit L: ValueOf[L]): String = valueOf[L]

  override def toString: String = s"Labelled($value)"
}

object Labelled {
  sealed class Apply[L <: String with Singleton] {
    def apply[A](a: A): Labelled[L, A] = new Labelled[L, A](a)
  }

  def apply[L <: String with Singleton]: Apply[L] = new Apply[L]

  def unapply[L <: String with Singleton, A](l: Labelled[L, A]): Option[A] = Some(l.value)
}

trait LabelledGeneric[A] {
  // NOTE: these methods are the same as Generic. However LabelledGeneric is not inherited from Generic,
  //       because it is not good that passing LabelledGeneric to a method needs Generic.
  type Repr
  def embed(a: A): Repr
  def project(r: Repr): A

  type Label <: String with Singleton
  @inline final def label(implicit L: ValueOf[Label]): String = valueOf[Label]
}

object LabelledGeneric {
  import scala.language.experimental.macros

  type Aux[A, L <: String with Singleton, R] = LabelledGeneric[A] {
    type Repr = R
    type Label = L
  }

  def apply[A](implicit lg: LabelledGeneric[A]): Aux[A, lg.Label, lg.Repr] = lg

  implicit def materialize[A, L <: String with Singleton, R]: Aux[A, R, L] =
    macro GenericMacros.materializeLabelled[A, L, R]

  implicit val unit: LabelledGeneric.Aux[Unit, "Unit", HNil] = new LabelledGeneric[Unit] {
    type Repr = HNil
    def embed(a: Unit): Repr = HNil
    def project(r: Repr): Unit = ()
    type Label = "Unit"
  }
}
