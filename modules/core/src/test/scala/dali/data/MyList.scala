package dali
package data

sealed abstract class MyList[+A] { self =>
  def toList: List[A] = self match {
    case MyNil()            => Nil
    case MyCons(head, tail) => head :: tail.toList
  }
}

object MyList {
  def apply[A](vs: A*): MyList[A] = fromList(vs.toList)

  def fromList[A](l: List[A]): MyList[A] =
    l.foldRight(MyNil[A]: MyList[A])(MyCons(_, _))
}

case class MyNil[+A] private () extends MyList[A]
case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]
