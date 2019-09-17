package codes.quine.labo
package dali

import minitest.SimpleTestSuite

object CoproductSuite extends SimpleTestSuite {
  test("Coproduct: construction") {
    assert(Coproduct[Int :+: CNil](1).isInstanceOf[Int :+: CNil])
    assert(Coproduct[Int :+: String :+: CNil]("foo").isInstanceOf[Int :+: String :+: CNil])

    assertDoesNotCompile(
      """Coproduct[Int :+: CNil]("foo")""",
      """Int :\+: .*CNil does not contain String. \(implicit not found\)"""
    )
    assertDoesNotCompile(
      """Coproduct[Int :+: Int :+: CNil](1)""",
      """Int :\+: Int :\+: .*CNil contains Int twice or more times. \(implicit ambiguous\)"""
    )
  }

  test("Coproduct: ==") {
    assert(Coproduct[Int :+: CNil](1) == Inl(1))
    assert(Coproduct[Int :+: String :+: CNil]("foo") == Inr(Inl("foo")))
  }

  test("Coproduct: toString") {
    assertEquals(Coproduct[Int :+: CNil](1).toString, "Inl(1)")
    assertEquals(Coproduct[Int :+: String :+: CNil]("foo").toString, "Inr(Inl(foo))")
  }

  test("Coproduct: pattern matching") {
    assert(Coproduct[Int :+: CNil](1) match {
      case Inl(i) => i == 1
      case _      => false
    })

    assert(Coproduct[Int :+: String :+: CNil]("foo") match {
      case Inr(Inl(s)) => s == "foo"
      case _           => false
    })
  }

  test("Coproduct: unsafeApply") {
    assert(Coproduct.unsafeApply(0, 1).asInstanceOf[Int :+: CNil] == Inl(1))
    assert(Coproduct.unsafeApply(1, 1).asInstanceOf[String :+: Int :+: CNil] == Inr(Inl(1)))
  }

  test("Coproduct: unsafeGet") {
    assert(Coproduct.unsafeGet(Coproduct[Int :+: CNil](1)).asInstanceOf[Int] == 1)
    assert(Coproduct.unsafeGet(Coproduct[Int :+: String :+: CNil]("foo")).asInstanceOf[String] == "foo")
    assert(Coproduct.unsafeGet(Coproduct.unsafeApply(5000, "foo")).asInstanceOf[String] == "foo")
  }
}
