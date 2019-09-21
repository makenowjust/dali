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

There are `cats` typeclasses whose instance can be derivated by `dali-cats`:

- `Show`
- `Eq`
- `PartialOrder`
- `Order`†
- `SemigroupK`†
- `MonoidK`†
- `Foldable`
- `Functor`
- `Traverse`
- `Apply`†
- `Applicative`†
- `Alternative`†
- `FlatMap`†
- `Monad`†

(† auto derivation for the typeclass marked this symbol only supports `case class`.)

[cats]: https://typelevel.org/cats/
