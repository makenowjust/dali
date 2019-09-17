dali-cats
====

> [cats] typeclasses instance derivation for simple ADTs.

Usage
----

```scala
scala> import cats._, dali.cats.derive.functor._
import cats._
import dali.cats.derive.functor._

scala> :paste
// Entering paste mode (ctrl-D to finish)

sealed trait MyList[A]
case class MyNil[A]() extends MyList[A]
case class MyCons[A](head: A, tail: MyList[A]) extends MyList[A]

// Exiting paste mode, now interpreting.

defined trait MyList
defined class MyNil
defined class MyCons

scala> Functor[MyList] // derive Functor instance for MyList.
res0: cats.Functor[MyList] = dali.cats.DeriveFunctor$$anon$8@129d92e6

scala> Functor[MyList].map(MyCons(1, MyNil[Int]))(_ + 1)
res1: MyList[Int] = MyCons(2,MyNil())
```

Status
----

`dali-cats` provides auto derivations for these cats typeclasses:

- `Show`
- `SemigroupK` (only for `case class`)
- `MonoidK` (only for `case class`)
- `Foldable`
- `Functor`
- `Traverse`
- `Apply` (only for `case class`)
- `Applicative` (only for `case class`)
- `Alternative` (only for `case class`)
- `FlatMap` (only for `case class`)
- `Monad` (only for `case class`)

[cats]: https://typelevel.org/cats/