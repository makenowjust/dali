package dali
package higher

import macros.GenericMacros

trait Generic1[F[_]] {
  type Repr1 <: TypeFunction1

  def embed[A](fa: F[A]): Repr1#Apply[A]
  def project[A](ra: Repr1#Apply[A]): F[A]
}

object Generic1 {
  import scala.language.experimental.macros

  type Aux[F[_], R <: TypeFunction1] = Generic1[F] { type Repr1 = R }

  def apply[F[_]](implicit g: Generic1[F]): Aux[F, g.Repr1] = g

  implicit def materialize[F[_], R <: TypeFunction1]: Aux[F, R] = macro GenericMacros.materialize1[F, R]
}
