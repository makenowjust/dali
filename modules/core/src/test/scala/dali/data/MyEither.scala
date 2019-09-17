package dali
package data

sealed trait MyEither[A, B]
case class MyLeft[A, B](left: A) extends MyEither[A, B]
case class MyRight[A, B](right: B) extends MyEither[A, B]
