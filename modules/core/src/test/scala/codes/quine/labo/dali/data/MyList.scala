package codes.quine.labo
package dali
package data

sealed abstract class MyList[+A]
case class MyNil[+A] private () extends MyList[A]
case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]
