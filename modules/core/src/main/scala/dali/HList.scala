package dali

/**
 * HList is the base class of heterogeneous list (a.k.a. product of types.)
 *
 * One of the examples of HList is: {{{Int :*: String :*: HNil}}}
 * It is a pair of Int and String values.
 *
 * In fact HList looks like tuples, but the length of types is variable.
 * For instance `Int :*: String :*: Boolean :*: HNil` is the type just containing 3 types values.
 *
 * For HList construction, use [[HNil]] and [[:*:]]:
 *
 * {{{
 * scala> 1 :*: "foo" :*: true :*: HNil
 * res0: Int :*: String :*: Boolean :*: HNil = 1 :*: foo :*: true :*: HNil
 * }}}
 *
 * And, use pattern matching for extraction:
 *
 * {{{
 * scala> res1 match {
 *      |   case h :*: t => (h, t)
 *      | }
 * res1: (Int, String :*: Boolean :*: HNil) =  (1, foo :*: true :*: HNil)
 * }}}
 *
 * NOTE: [[HList.Ops#:*: :*:]] method for any HList instance is defined in [[HList.Ops]] class for some reason,
 *       and HList instance can call this via [[HList$ implicit conversion]].
 */
sealed trait HList extends Serializable with Product

/** HNil is the empty [[HList]] (a.k.a. Unit.) */
sealed trait HNil extends HList {
  // Override :*: for hacking for fixing type.
  // (e.g. `1 :*: HNil` is typed as `Int :*: HNil.type` if this override is missing.)
  def :*:[H](h: H): H :*: HNil = dali.:*:(h, this)
  override def toString: String = "HNil"
}

case object HNil extends HNil

/** :*: is a constructor of HList types list. */
final case class :*:[+H, +T <: HList](head: H, tail: T) extends HList {
  override def toString: String = s"$head :*: $tail"
}

object HList {
  implicit final class Ops[L <: HList](private val l: L) extends AnyVal {
    def :*:[H](h: H): H :*: L = dali.:*:(h, l)
  }
}
