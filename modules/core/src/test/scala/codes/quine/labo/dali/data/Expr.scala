package codes.quine.labo
package dali
package data

sealed trait Expr
object Var extends Expr
case class Val(x: Int) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
