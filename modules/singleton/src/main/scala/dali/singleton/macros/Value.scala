package dali
package singleton
package macros

sealed abstract class Value(val typeName: String, val valueToString: String) {
  override def toString: String = s"$valueToString: $typeName"
}

object Value {
  final case class IntValue(value: Int) extends Value("Int", value.toString)
  final case class LongValue(value: Long) extends Value("Long", value.toString)
  final case class FloatValue(value: Float) extends Value("Float", value.toString)
  final case class DoubleValue(value: Double) extends Value("Double", value.toString)
  final case class BooleanValue(value: Boolean) extends Value("Boolean", value.toString)
  final case class StringValue(value: String) extends Value("String", value.toString)

  final case class ValueException(private val message: String, private val cause: Throwable = null)
      extends Exception(message, cause)

  def toInt(x: Value): Value = x match {
    case IntValue(x)    => IntValue(x)
    case LongValue(x)   => IntValue(x.toInt)
    case FloatValue(x)  => IntValue(x.toInt)
    case DoubleValue(x) => IntValue(x.toInt)
    case StringValue(x) =>
      x.toIntOption.map(IntValue(_)).getOrElse(throw new ValueException(s"failed on parsing Int: $x"))
    case _ => throw new ValueException(s"invalid operation: toInt[$x]")
  }

  def toLong(x: Value): Value = x match {
    case IntValue(x)    => LongValue(x)
    case LongValue(x)   => LongValue(x)
    case FloatValue(x)  => LongValue(x.toLong)
    case DoubleValue(x) => LongValue(x.toLong)
    case StringValue(x) =>
      x.toLongOption.map(LongValue(_)).getOrElse(throw new ValueException(s"failed on parsing Long: $x"))
    case _ => throw new ValueException(s"invalid operation: toLong[$x]")
  }

  def toFloat(x: Value): Value = x match {
    case IntValue(x)    => FloatValue(x)
    case LongValue(x)   => FloatValue(x)
    case FloatValue(x)  => FloatValue(x)
    case DoubleValue(x) => FloatValue(x.toFloat)
    case StringValue(x) =>
      x.toFloatOption.map(FloatValue(_)).getOrElse(throw new ValueException(s"failed on parsing Float: $x"))
    case _ => throw new ValueException(s"invalid operation: toFloat[$x]")
  }

  def toDouble(x: Value): Value = x match {
    case IntValue(x)    => DoubleValue(x)
    case LongValue(x)   => DoubleValue(x)
    case FloatValue(x)  => DoubleValue(x)
    case DoubleValue(x) => DoubleValue(x)
    case StringValue(x) =>
      x.toDoubleOption.map(DoubleValue(_)).getOrElse(throw new ValueException(s"failed on parsing Double: $x"))
    case _ => throw new ValueException(s"invalid operation: toDouble[$x]")
  }

  def add(x: Value, y: Value): Value = (x, y) match {
    case (IntValue(x), IntValue(y))       => IntValue(x + y)
    case (LongValue(x), LongValue(y))     => LongValue(x + y)
    case (FloatValue(x), FloatValue(y))   => FloatValue(x + y)
    case (DoubleValue(x), DoubleValue(y)) => DoubleValue(x + y)
    case _                                => throw new ValueException(s"invalid operation: ($x) + ($y)")
  }

  def sub(x: Value, y: Value): Value = (x, y) match {
    case (IntValue(x), IntValue(y))       => IntValue(x - y)
    case (LongValue(x), LongValue(y))     => LongValue(x - y)
    case (FloatValue(x), FloatValue(y))   => FloatValue(x - y)
    case (DoubleValue(x), DoubleValue(y)) => DoubleValue(x - y)
    case _                                => throw new ValueException(s"invalid operation: ($x) - ($y)")
  }

  def mul(x: Value, y: Value): Value = (x, y) match {
    case (IntValue(x), IntValue(y))       => IntValue(x * y)
    case (LongValue(x), LongValue(y))     => LongValue(x * y)
    case (FloatValue(x), FloatValue(y))   => FloatValue(x * y)
    case (DoubleValue(x), DoubleValue(y)) => DoubleValue(x * y)
    case _                                => throw new ValueException(s"invalid operation: ($x) * ($y)")
  }

  def div(x: Value, y: Value): Value = (x, y) match {
    case (IntValue(x), IntValue(y))       => IntValue(x / y)
    case (LongValue(x), LongValue(y))     => LongValue(x / y)
    case (FloatValue(x), FloatValue(y))   => FloatValue(x / y)
    case (DoubleValue(x), DoubleValue(y)) => DoubleValue(x / y)
    case _                                => throw new ValueException(s"invalid operation: ($x) / ($y)")
  }

  def mod(x: Value, y: Value): Value = (x, y) match {
    case (IntValue(x), IntValue(y))       => IntValue(x % y)
    case (LongValue(x), LongValue(y))     => LongValue(x % y)
    case (FloatValue(x), FloatValue(y))   => FloatValue(x % y)
    case (DoubleValue(x), DoubleValue(y)) => DoubleValue(x % y)
    case _                                => throw new ValueException(s"invalid operation: ($x) % ($y)")
  }

  def eq(x: Value, y: Value): Value = (x, y) match {
    case (IntValue(x), IntValue(y))         => BooleanValue(x == y)
    case (LongValue(x), LongValue(y))       => BooleanValue(x == y)
    case (FloatValue(x), FloatValue(y))     => BooleanValue(x == y)
    case (DoubleValue(x), DoubleValue(y))   => BooleanValue(x == y)
    case (BooleanValue(x), BooleanValue(y)) => BooleanValue(x == y)
    case (StringValue(x), StringValue(y))   => BooleanValue(x == y)
    case _                                  => throw new ValueException(s"invalid operation: ($x) == ($y)")
  }
}
