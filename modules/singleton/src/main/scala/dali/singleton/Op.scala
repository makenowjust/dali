package dali
package singleton

sealed trait Op

object Op {
  sealed trait Unary extends Op
  sealed trait ToInt extends Unary
  sealed trait ToLong extends Unary
  sealed trait ToFloat extends Unary
  sealed trait ToDouble extends Unary

  sealed trait Binary extends Op
  sealed trait Add extends Binary
  sealed trait Sub extends Binary
  sealed trait Mul extends Binary
  sealed trait Div extends Binary
  sealed trait Mod extends Binary
  sealed trait Eq extends Binary
}
