package dali
package singleton

sealed trait Expr

object Expr {
  sealed trait Unary[O <: Op.Unary, E] extends Expr
  sealed trait Binary[O <: Op.Binary, E, F] extends Expr
}
