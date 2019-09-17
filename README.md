dali
====

> Modern, Lightweight and Simple Generic Programming Library
>
> (i.e. [shapeless] alternative for Scala 2.13 era)

Features
----

- **Modern**: designed for Scala 2.13.
- **Lightweight**: provides only few things, `HList`, `Coproduct`, `Generic` and higer variants.
- **Simple**: `Generic1` derives `HList1` and `Coproduct1` simply.

Modules
----

- [dali-core](modules/core): core module of dali.
- [dali-cats](modules/cats): [cats] typeclasses instance derivation for simple ADTs.

License
----

MIT License
2019 (C) TSUYUSATO "[MakeNowJust]" Kitsune

[MakeNowJust]: https://github.com/MakeNowJust
[shapeless]: https://github.com/milessabin/shapeless
[cats]: https://typelevel.org/cats/