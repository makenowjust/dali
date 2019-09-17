package dali

import scala.annotation.{implicitAmbiguous, implicitNotFound, tailrec}

/**
 * Coproduct is the base class of coproduct of types.
 *
 * One of the examples of Coproduct is: {{{Int :+: String :+: CNil}}}
 * This type instance has either Int or String value.
 *
 * In fact Coproduct looks like Either, but the length of types is variable.
 *
 * For Coproduct construction, use [[Coproduct$#apply Coproduct]] with type parameter:
 *
 * {{{
 * scala> Coproduct[Int :+: String :+: Boolean :+: CNil]("foo")
 * res0: Int :+: String :+: Boolean :+: CNil = Inr(Inl(foo))
 * }}}
 *
 * NOTE: On the above example, the type parameter takes Coproduct cannot be inferred by the compiler,
 *       so it is necessary to compile it.
 *
 * If the type parameter takes Coproduct does not contain the value type, a compilation fails:
 *
 * {{{
 * scala> Coproduct[Int :+: String :+: CNil](true)
 *
 *        error: Int :+: String :+: CNil does not contain Boolean. (implicit not found)
 * }}}
 *
 * Or the type parameter contains the value type twice or more times, a compilation fails also:
 *
 * {{{
 * scala> Coproduct[Int :+: Int :+: CNil](1)
 *
 *        error: Int :+: Int :+: CNil contains two or more Int. (implicit ambiguous)
 * }}}
 */
sealed trait Coproduct extends Serializable with Product

/** CNil is the empty [[Coproduct]] (a.k.a. Void.) */
sealed trait CNil extends Coproduct

/** :+: is constructor of Coproduct types list. */
sealed trait :+:[+H, +T <: Coproduct] extends Coproduct

/** Inl is H :+: T instance when H value is available. */
final case class Inl[+H, +T <: Coproduct](head: H) extends :+:[H, T]

/** Inr is H :+: T instance when T value is available. */
final case class Inr[+H, +T <: Coproduct](tail: T) extends :+:[H, T]

object Coproduct {
  def unsafeApply(n: Int, v: Any): Coproduct =
    (0 until n).foldLeft[Coproduct](Inl(v))((acc, _) => Inr(acc))

  @tailrec
  def unsafeGet(c: Coproduct): Any = c match {
    case Inl(h) => h
    case Inr(t) => unsafeGet(t)
  }

  sealed class Apply[C <: Coproduct] {
    def apply[A](a: A)(implicit i: Inject[A, C]): C = i.inject(a)
  }

  def apply[C <: Coproduct]: Apply[C] = new Apply[C]

  @implicitNotFound("${C} does not contain ${A}. (implicit not found)")
  trait Inject[A, C <: Coproduct] {
    def inject(a: A): C
  }

  object Inject extends Inject0

  private[dali] trait Inject0 extends Inject1 {
    implicit def injectTail[A, B, C <: Coproduct](implicit i: Inject[A, C]): Inject[A, B :+: C] =
      new Inject[A, B :+: C] {
        def inject(a: A): B :+: C = Inr(i.inject(a))
      }
  }

  private[dali] trait Inject1 {
    @implicitAmbiguous("${A} :+: ${C} contains ${A} twice or more times. (implicit ambiguous)")
    implicit def injectHead[A, C <: Coproduct]: Inject[A, A :+: C] = new Inject[A, A :+: C] {
      def inject(a: A): A :+: C = Inl(a)
    }
  }
}
