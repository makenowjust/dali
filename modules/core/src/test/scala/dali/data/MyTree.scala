package dali
package data

case class MyTree[A](root: A, children: MyList[MyTree[A]])
