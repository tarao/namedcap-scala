package com.github.tarao.namedcap

import org.scalatest.{FunSpec, Matchers, OptionValues, Inside, Inspectors}

object PatternExample extends Pattern.Interpolation.Implicits {
  import Pattern._
  case object Url extends Group("https?://.*")
  case object Identifier extends Group("[a-zA-Z0-9][a-zA-Z0-9_-]*")
  case object UnsignedNumber extends Group("[1-9][0-9]*")
  case object Sign extends Group("[-+]")
  case object SignedNumber extends NestedGroup(pattern"$Sign$UnsignedNumber")

}

class PatternSpec extends FunSpec with Matchers
    with OptionValues with Inside with Inspectors {
  import PatternExample._

  describe("Pattern") {
    val p1 = pattern"/foo/$Identifier/$Url"
    val p2 = pattern"/bar/$SignedNumber"

    it("should be instantiated by the interpolation") {
      p1.r.toString shouldBe "/foo/([a-zA-Z0-9][a-zA-Z0-9_-]*)/(https?://.*)"
      p2.r.toString shouldBe "/bar/(([-+])([1-9][0-9]*))"
    }

    describe(".groupNames") {
      p1.groupNames should contain theSameElementsAs Seq("Identifier", "Url")
      p2.groupNames should contain theSameElementsAs Seq("SignedNumber")
    }

    describe(".allGroupNames") {
      p1.allGroupNames should contain theSameElementsAs Seq(
        "Identifier",
        "Url"
      )
      p2.allGroupNames should contain theSameElementsAs Seq(
        "SignedNumber",
        "Sign",
        "UnsignedNumber"
      )
    }

    describe(".namedGroups") {
      p1.namedGroups.map(pair => pair._1 -> pair._2.r.toString) should
        contain theSameElementsAs Seq(
          "Identifier" -> "([a-zA-Z0-9][a-zA-Z0-9_-]*)",
          "Url" -> "(https?://.*)"
        )
      p2.namedGroups.map(pair => pair._1 -> pair._2.r.toString) should
        contain theSameElementsAs Seq(
          "SignedNumber" -> "(([-+])([1-9][0-9]*))"
        )
    }

    describe(".allNamedGroups") {
      p1.allNamedGroups.map(pair => pair._1 -> pair._2.r.toString) should
        contain theSameElementsAs Seq(
          "Identifier" -> "([a-zA-Z0-9][a-zA-Z0-9_-]*)",
          "Url" -> "(https?://.*)"
        )
      p2.allNamedGroups.map(pair => pair._1 -> pair._2.r.toString) should
        contain theSameElementsAs Seq(
          "SignedNumber" -> "(([-+])([1-9][0-9]*))",
          "Sign" -> "([-+])",
          "UnsignedNumber" -> "([1-9][0-9]*)"
        )
    }

    describe(".mapGroupsIn") {
      it("should replace the matched portions") {
        val i1 = "/foo/hoge/http://example.com/"
        p1.mapGroupsIn(i1)(g => s => s"<$g>$s</$g>") shouldBe
          "/foo/<Identifier>hoge</Identifier>/<Url>http://example.com/</Url>"

        val i2 = "/bar/-12345"
        p2.mapGroupsIn(i2)(g => s => s"<$g>$s</$g>") shouldBe
          "/bar/<SignedNumber><Sign>-</Sign><UnsignedNumber>12345</UnsignedNumber></SignedNumber>"

      }
    }

    describe(".toString") {
      p1.toString shouldBe "/foo/${Identifier}/${Url}"
      p2.toString shouldBe "/bar/${SignedNumber}"
    }

    describe("+") {
      (p1 + "hoge").r.toString shouldBe
        "/foo/([a-zA-Z0-9][a-zA-Z0-9_-]*)/(https?://.*)hoge"
      (p1 + "hoge").toString shouldBe "/foo/${Identifier}/${Url}hoge"
      (p1 + p2).r.toString shouldBe
        "/foo/([a-zA-Z0-9][a-zA-Z0-9_-]*)/(https?://.*)/bar/(([-+])([1-9][0-9]*))"
      (p1 + p2).toString shouldBe "/foo/${Identifier}/${Url}/bar/${SignedNumber}"
    }

    it("should be able to embed a pattern into another pattern") {
      val p3 = pattern"/baz$p1"
      p3.r.toString shouldBe
        "/baz/foo/([a-zA-Z0-9][a-zA-Z0-9_-]*)/(https?://.*)"
      p3.toString shouldBe "/baz/foo/${Identifier}/${Url}"
      p3.groupNames should contain theSameElementsAs Seq("Identifier", "Url")
      p3.namedGroups.map(pair => pair._1 -> pair._2.r.toString) should
        contain theSameElementsAs Seq(
          "Identifier" -> "([a-zA-Z0-9][a-zA-Z0-9_-]*)",
          "Url" -> "(https?://.*)"
        )
      p3.allGroupNames should contain theSameElementsAs Seq(
        "Identifier",
        "Url"
      )
      p3.allNamedGroups.map(pair => pair._1 -> pair._2.r.toString) should
        contain theSameElementsAs Seq(
          "Identifier" -> "([a-zA-Z0-9][a-zA-Z0-9_-]*)",
          "Url" -> "(https?://.*)"
        )

      val p4 = pattern"$p1 or $p2"
      p4.r.toString shouldBe
        "/foo/([a-zA-Z0-9][a-zA-Z0-9_-]*)/(https?://.*) or /bar/(([-+])([1-9][0-9]*))"
      p4.toString shouldBe
        "/foo/${Identifier}/${Url} or /bar/${SignedNumber}"
      p4.groupNames should contain theSameElementsAs Seq(
        "Identifier",
        "Url",
        "SignedNumber"
      )
      p4.namedGroups.map(pair => pair._1 -> pair._2.r.toString) should
        contain theSameElementsAs Seq(
          "Identifier" -> "([a-zA-Z0-9][a-zA-Z0-9_-]*)",
          "Url" -> "(https?://.*)",
          "SignedNumber" -> "(([-+])([1-9][0-9]*))"
        )
      p4.allGroupNames should contain theSameElementsAs Seq(
        "Identifier",
        "Url",
        "SignedNumber",
        "Sign",
        "UnsignedNumber"
      )
      p4.allNamedGroups.map(pair => pair._1 -> pair._2.r.toString) should
        contain theSameElementsAs Seq(
          "Identifier" -> "([a-zA-Z0-9][a-zA-Z0-9_-]*)",
          "Url" -> "(https?://.*)",
          "SignedNumber" -> "(([-+])([1-9][0-9]*))",
          "Sign" -> "([-+])",
          "UnsignedNumber" -> "([1-9][0-9]*)"
        )
    }
  }
}
