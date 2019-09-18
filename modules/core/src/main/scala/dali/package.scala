package dali

trait Label[L <: String with Singleton]

object `package` {
  type Labelled[L <: String with Singleton, A] = A with Label[L]

  object Labelled {
    class Apply[L <: String with Singleton] {
      def apply[A](a: A): Labelled[L, A] = a.asInstanceOf[Labelled[L, A]]
    }

    def apply[L <: String with Singleton]: Apply[L] = new Apply[L]
    def unapply[A](a: A): Option[A] = Some(a)
  }
}
