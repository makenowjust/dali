package dali

package object singleton {
  type =::=[A, V] = Valuable.Aux[A, V]

  type toInt[E] = Expr.Unary[Op.ToInt, E]
  type toLong[E] = Expr.Unary[Op.ToLong, E]
  type toFloat[E] = Expr.Unary[Op.ToFloat, E]
  type toDouble[E] = Expr.Unary[Op.ToDouble, E]

  type +[E, F] = Expr.Binary[Op.Add, E, F]
  type -[E, F] = Expr.Binary[Op.Sub, E, F]
  type *[E, F] = Expr.Binary[Op.Mul, E, F]
  type /[E, F] = Expr.Binary[Op.Div, E, F]
  type %[E, F] = Expr.Binary[Op.Mod, E, F]
}
