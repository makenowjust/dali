package dali
package singleton

trait HListAt[I <: Int with Singleton, L <: HList, X] {
  def apply(l: L): X
}

object HListAt {
  implicit def head[H, T <: HList]: HListAt[0, H :*: T, H] = new HListAt[0, H :*: T, H] {
    def apply(l: H :*: T): H = l.head
  }

  implicit def tail[I1 <: Int with Singleton, I0 <: Int with Singleton: Valuable.Aux[I1 - 1, *], H, T <: HList, X](implicit at: HListAt[I0, T, X]): HListAt[I1, H :*: T, X] =
    new HListAt[I1, H :*: T, X] {
      def apply(l: H :*: T): X = at(l.tail)
    }

  implicit class HListOps[L <: HList](l: L) {
    def at[I <: Int with Singleton, X](i: I)(implicit at: HListAt[I, L, X]): X = at(l)
  }
}
