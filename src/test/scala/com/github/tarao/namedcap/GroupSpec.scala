package com.github.tarao.namedcap

import org.scalatest.{OptionValues, Inside, Inspectors}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class GroupSpec extends AnyFunSpec with Matchers
    with OptionValues with Inside with Inspectors
    with Implicits {
  describe("Constructor method") {
    it("should give a specified name") {
      val g1 = Group("foo", "[a-zA-Z0-9]+")
      g1.name shouldBe "foo"
      g1.pattern shouldBe "[a-zA-Z0-9]+"
      g1.names() shouldBe Seq("foo")
      g1.allNames() shouldBe Seq("foo")

      val p1 = pattern"$g1:$g1"

      val g2 = NestedGroup("bar", p1)
      g2.name shouldBe "bar"
      g2.pattern shouldBe "([a-zA-Z0-9]+):([a-zA-Z0-9]+)"
      g2.names() shouldBe Seq("bar")
      g2.allNames() shouldBe Seq("bar", "foo", "foo")

      val p2 = pattern"$g2-$g2"
      val g3 = NestedGroup("baz", p2)
      g3.name shouldBe "baz"
      g3.pattern shouldBe "(([a-zA-Z0-9]+):([a-zA-Z0-9]+))-(([a-zA-Z0-9]+):([a-zA-Z0-9]+))"
      g3.names() shouldBe Seq("baz")
      g3.allNames() shouldBe Seq("baz", "bar", "foo", "foo", "bar", "foo", "foo")

      val g4 = UnnamedGroup(p1)
      g4.name shouldBe "UnnamedGroup(${foo}:${foo})"
      g4.pattern shouldBe "([a-zA-Z0-9]+):([a-zA-Z0-9]+)"
      g4.names() shouldBe Seq("foo", "foo")
      g4.allNames() shouldBe Seq("foo", "foo")

      val g5 = UnnamedGroup(p2)
      g5.name shouldBe "UnnamedGroup(${bar}-${bar})"
      g5.pattern shouldBe "(([a-zA-Z0-9]+):([a-zA-Z0-9]+))-(([a-zA-Z0-9]+):([a-zA-Z0-9]+))"
      g5.names() shouldBe Seq("bar", "bar")
      g5.allNames() shouldBe Seq("bar", "foo", "foo", "bar", "foo", "foo")
    }
  }
}
