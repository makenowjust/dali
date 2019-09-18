package dali

import macros._

/**
 * Generic is typeclass that converts between simple ADT and [[HList]] or [[Coproduct]] values.
 *
 * This instance is derived by macro, so it is unnecessary to define this instance by your hand.
 *
 * Case classes are encoded as HList.
 *
 * {{{
 * scala> case class Foo(x: Int, y: String)
 * defined class Foo
 *
 * scala> :t Generic[Foo]
 * Generic[Foo]{type Repr = Int :*: String :*: HNil}
 *
 * scala> Generic[Foo].embed(Foo(42, "foo"))
 * res0: Int :*: String :*: HNil = 42 :*: foo :*: HNil
 *
 * scala> Generic[Foo].project(res0)
 * res1: Foo = Foo(42,foo)
 * }}}
 *
 * Sealed traits are encoded as Coproduct.
 *
 * {{{
 * scala> :paste
 * // Entering paste mode (ctrl-D to finish)
 *
 * sealed trait Bar
 * case class Bar1(x: Int) extends Bar
 * case class Bar2(y: String) extends Bar
 *
 * // Exiting paste mode, now interpreting.
 *
 * defined trait Bar
 * defined class Bar1
 * defined class Bar2
 *
 * scala> :t Generic[Bar]
 * Generic[Bar]{type Repr = Bar1 :+: Bar2 :+: CNil}
 *
 * scala> Generic[Bar].embed(Bar2("foo"))
 * res2: Bar1 :+: Bar2 :+: CNil = Inr(Inl(Bar2(foo)))
 *
 * scala> Generic[Bar].project(res2)
 * res3: Bar = Bar2(foo)
 * }}}
 */
trait Generic[A] {

  /** [[HList]] or [[Coproduct]] type that represents type A. */
  type Repr

  /** Convert type A value into Repr */
  def embed(a: A): Repr

  /** Convert Repr into type A value */
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
