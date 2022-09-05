package com.github.tarao.namedcap

import org.scalatest.{OptionValues, Inside, Inspectors}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MultiMapSpec extends AnyFunSpec with Matchers
    with OptionValues with Inside with Inspectors {
  describe("MultiMap") {
    describe("Constructor") {
      it("should be able to instantiated as an empty map") {
        val m1 = MultiMap()
        m1 shouldBe empty
        m1.size shouldBe 0
        m1.iterator.toList shouldBe empty

        val m2 = MultiMap.empty
        m2 shouldBe empty
        m2.size shouldBe 0
        m2.iterator.toList shouldBe empty
      }

      it("should be instantiated from a scala.collection.immutable.Map[]") {
        val m = MultiMap(Map(
          "foo" -> Seq("a", "b"),
          "bar" -> Seq("c")
        ))
        m should not be empty
        m.size shouldBe 2
      }

      it("should be instantiated from a scala.collection.mutable.Map[]") {
        import scala.collection.mutable.{Map, ListBuffer}
        val m = MultiMap(Map(
          "foo" -> ListBuffer("a", "b"),
          "bar" -> ListBuffer("c")
        ))
        m should not be empty
        m.size shouldBe 2
      }

      it("shuold skip empty value") {
        val m = MultiMap(Map(
          "foo" -> Seq("a", "b"),
          "bar" -> Seq.empty,
          "baz" -> Seq("c")
        ))
        m should not be empty
        m.size shouldBe 2
        m.contains("bar") shouldBe false
      }
    }

    describe(".apply()") {
      it("should return the element") {
        val m = MultiMap(Map(
          "foo" -> Seq("a"),
          "bar" -> Seq("b")
        ))
        m("foo") shouldBe "a"
        m("bar") shouldBe "b"
      }

      it("should return the first element") {
        val m = MultiMap(Map(
          "foo" -> Seq("a", "b"),
          "baz" -> Seq("c")
        ))
        m("foo") shouldBe "a"
        m("baz") shouldBe "c"
      }

      it("should not return for the empty sequence") {
        val m = MultiMap(Map(
          "foo" -> Seq("a", "b"),
          "bar" -> Seq.empty,
          "baz" -> Seq("c")
        ))
        a[NoSuchElementException] should be thrownBy m("bar")
      }
    }

    describe(".get()") {
      it("should return the element") {
        val m = MultiMap(Map(
          "foo" -> Seq("a"),
          "bar" -> Seq("b")
        ))
        m.get("foo") shouldBe Some("a")
        m.get("bar") shouldBe Some("b")
      }

      it("should return the first element") {
        val m = MultiMap(Map(
          "foo" -> Seq("a", "b"),
          "baz" -> Seq("c")
        ))
        m.get("foo") shouldBe Some("a")
        m.get("baz") shouldBe Some("c")
      }

      it("should not return for the empty sequence") {
        val m = MultiMap(Map(
          "foo" -> Seq("a", "b"),
          "bar" -> Seq.empty,
          "baz" -> Seq("c")
        ))
        m.get("bar") shouldBe None
      }
    }

    describe(".getAll()") {
      it("should return the element") {
        val m = MultiMap(Map(
          "foo" -> Seq("a"),
          "bar" -> Seq("b")
        ))
        m.getAll("foo") shouldBe Seq("a")
        m.getAll("bar") shouldBe Seq("b")
      }

      it("should return all the elements") {
        val m = MultiMap(Map(
          "foo" -> Seq("a", "b"),
          "baz" -> Seq("c")
        ))
        m.getAll("foo") shouldBe Seq("a", "b")
        m.getAll("baz") shouldBe Seq("c")
      }

      it("should return an empty sequence for the missing key") {
        val m = MultiMap(Map(
          "foo" -> Seq("a", "b"),
          "bar" -> Seq.empty,
          "baz" -> Seq("c")
        ))
        m.getAll("bar") shouldBe empty
        m.getAll("qux") shouldBe empty
      }
    }

    describe("+") {
      it("should add an element") {
        val m1 = MultiMap(Map(
          "foo" -> Seq("a", "b"),
          "bar" -> Seq.empty,
          "baz" -> Seq("c")
        ))
        val m2 = m1 + ("qux" -> "d") + ("bar" -> "e")
        m2("qux") shouldBe "d"
        m2("bar") shouldBe "e"
      }

      it("should add an element to an existing key") {
        val m1 = MultiMap(Map(
          "foo" -> Seq("a", "b"),
          "bar" -> Seq.empty,
          "baz" -> Seq("c")
        ))
        val m2 = m1 + ("foo" -> "d") + ("baz" -> "e")
        m2("foo") shouldBe "a"
        m2.getAll("foo") shouldBe Seq("a", "b", "d")
        m2("baz") shouldBe "c"
        m2.getAll("baz") shouldBe Seq("c", "e")
      }
    }

    describe("-") {
      it("should remove a key") {
        val m1 = MultiMap(Map(
          "foo" -> Seq("a", "b"),
          "bar" -> Seq.empty,
          "baz" -> Seq("c")
        ))
        val m2 = m1 - "foo" - "baz"
        m2.get("foo") shouldBe None
        m2.get("baz") shouldBe None
      }

      it("should remove a missing key") {
        val m1 = MultiMap(Map(
          "foo" -> Seq("a", "b"),
          "bar" -> Seq.empty,
          "baz" -> Seq("c")
        ))
        val m2 = m1 - "bar" - "qux"
        m2.get("bar") shouldBe None
        m2.get("qux") shouldBe None
      }
    }
  }
}
