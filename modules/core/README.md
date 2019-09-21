dali-core
====

> core module of dali.

[![ScalaDoc][scaladoc-badge]](https://javadoc.io/doc/codes.quine/dali-core_2.13)

It provides:

- `HList` and `Coproduct`: data types for abstract representation of simple ADTs.
- `Generic`: auto derivated converter between `HList`, `Coproduct` and simple ADTs.
- `HList1` and `Coproduct1`: higher kinded version of `HList` and `Coproduct` for higher-kinded ADTs.
- `Param1`, `Rec1` and `Const1`: other materials for abstract representation of higher-kinded ADTs.
- `Generic1`: higher-kinded variant of `Generic`.

Q&A
====

**Q.**
The one feature provided by [shapeless] is missing in dali. Why?

**A.**
shapeless is great generic programming library in Scala.
However it has too many features in the one library.
Dali picks up the few important features from shapeless, and drops many features intentionally.

For instance:

- **literal (singleton) types**: because Scala 2.13 support this officailly. Extra library is no longer needed.
- **first class lazy value**: this reason is the same as above. Scala 2.13 has by-name implicit parameter.
- **compilation test**: good, but *why this is included by generic programming library?* In addition, [minitest] has this feature instead.
- **lenses**: this is not duty of generic programming library.
- **many useful operators for `HList` and `Coproduct`**: it is useful in specific cases. However, in many cases, `HList` and `Coproduct` are processed by recursive functions.

But I (MakeNowJust) planed to support many useful operators as another library from `dali-core`, like `dali-ops`.

[shapeless]: https://github.com/milessabin/shapeless
[minitest]: https://github.com/monix/minitest
[scaladoc-badge]: https://img.shields.io/badge/ScalaDoc-Reference-black?style=for-the-badge&logo=scala&colorA=D01F28&logoColor=white
