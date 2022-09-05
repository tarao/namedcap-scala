namedcap [![Build Status][CI-img]][CI] [![Maven Central][maven-img]][maven]
========

Provides named capturing groups with the following enhanced features.

- Allows multiple groups with the same name
- Allows defining named groups as `case object`
- Provides methods to replace groups in a pattern or a matched instance
- Provides a string interpolation to make a pattern

Getting started
---------------

Add dependency in your `build.sbt` as the following.

```scala
    libraryDependencies ++= Seq(
      "com.github.tarao" %% "namedcap" % "1.0.0"
    )
```

The library is available on [Maven Central][maven].  Currently,
supported Scala versions are 2.13.

Basic usage
-----------

```scala
import com.github.tarao.namedcap.Group
import com.github.tarao.namedcap.Implicits._ // for string interpolation

case object Url extends Group("https?://.*")
case object Sha1 extends Group("[0-9a-f]{40}")

val p = pattern"/object/$Sha1/for/$Url"
val m = p("/object/6c250662d7ac79316ec20b50accd4723d2bf17a7/for/https://github.com/tarao/namedcap-scala/")

m(Sha1.name) // 6c250662d7ac79316ec20b50accd4723d2bf17a7
// or m("Sha1")

m(Url.name)  // https://github.com/tarao/namedcap-scala/
// or m("Url")
```

You can define a named capturing group by `Group`.  `case object
YourGroup extends Group(pattern)` defines a group of `pattern` as an
identifier `YourGroup`.  A matched portion for the group is accessible
by the same name as the `case object` (`"YourGroup"` in this case).
`val YourGroup = Group.apply("YourGroup", pattern)` has the same
effect if you want to avoid using `case object`.

`pattern` interpolation allows you to define a pattern containing
named capturing groups.  A pattern object returned by the
interpolation has `apply()` method to make a match against a string
and returns matched portions with capturing group names.

Nested groups
-------------

```scala
import com.github.tarao.namedcap.{Group, NestedGroup}
import com.github.tarao.namedcap.Implicits._ // for string interpolation

case object Day extends Group("[0-9]{2}")
case object Month extends Group("[0-9]{2}")
case object Year extends Group("[0-9]{4}")

case object FullDate extends NestedGroup(pattern"$Year-$Month-$Day")

val p = pattern"Since $FullDate"
val m = p("Since 2016-02-11")

m("Day")      // 11
m("Month")    // 02
m("Year")     // 2016
m("FullDate") // 2016-02-11
```

If you want to have a named capturing group within another group, use
`NestedGroup`.  `NestedGroup` takes a pattern (a value returned by
`pattern` interpolation) so you can embed arbitrary subgroups with
surrounding pattern expressions.

Multiple groups with the same name
----------------------------------

```scala
import com.github.tarao.namedcap.{Group, NestedGroup}
import com.github.tarao.namedcap.Implicits._ // for string interpolation

case object Day extends Group("[0-9]{2}")
case object Month extends Group("[0-9]{2}")
case object Year extends Group("[0-9]{4}")

case object FullDate extends NestedGroup(pattern"$Year-$Month-$Day")

val p = pattern"From $FullDate to $FullDate"
val m = p("From 1985-10-26 to 2015-10-21")

m("FullDate")        // 1985-10-26
m.getAll("FullDate") // Seq(1985-10-26, 2015-10-21)
```

A pattern can contain multiple groups with the same name (note that
this is not allowed with the standard
[`java.util.regex.Pattern`][java-regex] notation `(?<name>X)`).
Captured portions can be retrieve by `getAll()` of the match result.
In this case, `apply()` or `get()` on the match result is still
available and returns the first one.

Replacing matched portions in an instance string
------------------------------------------------

```scala
import com.github.tarao.namedcap.Group
import com.github.tarao.namedcap.Implicits._ // for string interpolation

case object Sha1 extends Group("[0-9a-f]{40}")

val p = pattern"commit $Sha1"
p.mapGroupsIn("commit 6c250662d7ac79316ec20b50accd4723d2bf17a7") { (g, s) =>
  if (g.name == "Sha1") s"""<a href="https://github.com/tarao/namedcap-scala/commit/$s">$s</a>"""
  else s
} // commit <a href="https://github.com/tarao/namedcap-scala/commit/6c250662d7ac79316ec20b50accd4723d2bf17a7">6c250662d7ac79316ec20b50accd4723d2bf17a7</a>
```

`mapGroupsIn()` method on a pattern replaces captured groups in an
matched instance.  This is useful especially when you want to
highlight the captured groups in the instance.

License
-------

- Copyright (C) INA Lintaro
- MIT License

[CI]: https://github.com/tarao/namedcap-scala/actions/workflows/ci.yaml
[CI-img]: https://github.com/tarao/namedcap-scala/actions/workflows/ci.yaml/badge.svg
[coverage]: https://coveralls.io/github/tarao/namedcap-scala?branch=master
[coverage-img]: https://coveralls.io/repos/tarao/namedcap-scala/badge.svg?branch=master&service=github
[maven]: https://search.maven.org/artifact/com.github.tarao/namedcap_2.13
[maven-img]: https://maven-badges.herokuapp.com/maven-central/com.github.tarao/namedcap_2.13/badge.svg

[java-regex]: https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html
