package com.github.tarao.namedcap

import scala.util.matching.Regex

/** A class to represent a group of pattern.
  *
  * This class is inteded to be used to declare a named pattern by
  * extending the class to a `case object`.  For example,
  *
  * {{{
  * case object SomeName extends Group("[a-z]+")
  * }}}
  *
  * `SomeName` defines a pattern matches with lower case alphabets
  * captured as a group.  The name of the group defaults to the value
  * of `SomeName.toString`.  Note that the default value of `toString`
  * for a `case object` is the name of the object, which is
  * `"SomeName"` in this example.
  */
abstract class Group(val pattern: String) {
  /** Returns the defined pattern. */
  def r: Regex = s"($pattern)".r

  /** Returns the group name */
  def name: String = toString

  /** Returns the group names of subpatterns. */
  def names(): Seq[String] = Seq(name)

  /** Returns subpatterns with their names. */
  def namedGroups(): Seq[(String, Group)] = Seq(name -> this)

  /** Returns all the group names of subpatterns. */
  def allNames(): Seq[String] = Seq(name)

  /** Returns all subpatterns with their names. */
  def allNamedGroups(): Seq[(String, Group)] = Seq(name -> this)

  def mapGroups(f: Group => String): String = f(this)
}
object Group {
  /** Make a group instance with a specific name.
    *
    * Use this method if you don't want to use `case object` to define
    * a group.
    */
  def apply(groupName: String, pattern: String): Group =
    new Group(pattern) { override def name: String = groupName }

  /** An empty group that has no name and matches with an empty string. */
  val empty = UnnamedGroup(Pattern(Seq.empty, Seq.empty))
}

/** A class to make a named pattern representation from a `Pattern`.
  *
  * This class is inteded to be used to declare a named pattern by
  * extending the class to a `case object`, with a `Pattern`, which
  * may include some named subpatterns.  For example, assuming that
  * `pattern` interpolation generates a `Pattern` with some named
  * patterns embedded,
  *
  * {{{
  * case object SomeName
  *     extends NestedGroup(pattern"/\$AnotherName/\$YetAnotherName")
  * }}}
  *
  * `SomeName` defines a named pattern matches with the whole pattern
  * captured as a group like one definition by `Pattern`.
  */
abstract class NestedGroup(p: Pattern) extends Group(p.r.toString) {
  override def allNames(): Seq[String] = super.allNames ++ p.allGroupNames
  override def allNamedGroups(): Seq[(String, Group)] =
    super.allNamedGroups ++ p.allNamedGroups
}
object NestedGroup {
  /** Make a nested group instance with a specific name.
    *
    * Use this method if you don't want to use `case object` to define
    * a nested group.
    */
  def apply(groupName: String, pattern: Pattern): Group =
    new NestedGroup(pattern) { override def name: String = groupName }
}

/** A class to make an unnamed pattern representation from a `Pattern`.
  *
  * This class is quite the same as `NestedGroup` except that the
  * whole pattern does neither have a name nor be captured as a group.
  */
case class UnnamedGroup(p: Pattern) extends Group(p.r.toString) {
  override def r: Regex = pattern.r
  override def names(): Seq[String] = p.groupNames
  override def namedGroups(): Seq[(String, Group)] = p.namedGroups
  override def allNames(): Seq[String] = p.allGroupNames
  override def allNamedGroups(): Seq[(String, Group)] = p.allNamedGroups
  override def mapGroups(f: Group => String): String = p.mapGroups(f)
}
