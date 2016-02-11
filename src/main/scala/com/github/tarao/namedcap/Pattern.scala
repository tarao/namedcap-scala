package com.github.tarao.namedcap

import scala.util.matching.Regex

/** A class to describe a pattern. */
case class Pattern(patterns: Seq[String], groups: Seq[Pattern.Group]) {
  def groupNames(): Seq[String] = groups.flatMap(_.names)
  def namedGroups(): Seq[(String, Pattern.Group)] =
    groups.flatMap(_.namedGroups)
  def allGroupNames(): Seq[String] = groups.flatMap(_.allNames)
  def allNamedGroups(): Seq[(String, Pattern.Group)] =
    groups.flatMap(_.allNamedGroups)

  def mapGroups(f: Pattern.Group => String): String =
    patterns.zipAll(groups.map(_.mapGroups(f)), "", "").map { zipped =>
      zipped._1 + zipped._2
    }.mkString

  def mapGroupsIn(instance: String)(
    f: Pattern.Group => String => String
  ): String = {
    import Pattern.RegexOps
    val indexToGroup: Map[Int, Pattern.Group] =
      allNamedGroups.zipWithIndex.map { case ((_, g), i) =>
        (i + 1) -> g
      }(scala.collection.breakOut)
    r.mapGroupsIn(instance)(f compose indexToGroup)
  }

  def r: Regex = mapGroups(_.r.toString).r

  def +(other: Pattern.Group): Pattern =
    Pattern(Seq("", ""), Seq(this, other))
  def +(other: String): Pattern = Pattern(Seq("", other), Seq(this))

  override def toString(): String = mapGroups {
    case Pattern.UnnamedGroup(p) => p.toString
    case g => s"$${${g.name}}"
  }
}
object Pattern {
  /** A class to represent a group of pattern.
    *
    * This class is inteded to declare a named pattern by extending the
    * class to a case object.  For example,
    *
    * {{{
    * case object SomeName extends Group("[a-z]+")
    * }}}
    *
    * `SomeName` defines a pattern matches with lower case alphabets
    * captured as a group.  The name of the group defaults to the value
    * of `SomeName.toString`.  Note that the default value of `toString`
    * for a case object is the name of the object, which is `"SomeName"`
    * in this example.
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
    val empty = UnnamedGroup(Pattern(Seq.empty, Seq.empty))
  }

  /** A class to make a named pattern representation from a `Pattern`.
    *
    * This class is inteded to declare a named pattern by extending the
    * class to a case object, with a `Pattern`, which may include some
    * named subpatterns.  For example, assuming that `pattern`
    * interpolation generates a `Pattern` with some named patterns
    * embedded,
    *
    * {{{
    * case object SomeName
    *     extends CompoundPattern(pattern"/\$AnotherName/\$YetAnotherName")
    * }}}
    *
    * `SomeName` defines a named pattern matches with the whole pattern
    * captured as a group like one definition by `Pattern`.
    */
  abstract class NestedGroup(p: Pattern) extends Group(p.r.toString) {
    override def allNames(): Seq[String] = name +: p.allGroupNames
    override def allNamedGroups(): Seq[(String, Group)] =
      super.allNamedGroups ++ p.allNamedGroups
  }

  /** A class to make an unnamed pattern representation from a `Pattern`.
    *
    * This class is quite the same as `CompoundPattern` except that the
    * whole pattern does neither have a name nor captured as a group.
    */
  case class UnnamedGroup(p: Pattern) extends Group(p.r.toString) {
    override def r: Regex = pattern.r
    override def names(): Seq[String] = p.groupNames
    override def namedGroups(): Seq[(String, Group)] = p.namedGroups
    override def allNames(): Seq[String] = p.allGroupNames
    override def allNamedGroups(): Seq[(String, Group)] = p.allNamedGroups
    override def mapGroups(f: Group => String): String = p.mapGroups(f)
  }

  implicit val patternToGroup: Pattern => Group = UnnamedGroup(_)

  /** An interpolation for named patterns. */
  class Interpolation(val sc: StringContext) extends AnyVal {
    /** Binds named patterns to their captured groups as a `Pattern`. */
    def pattern(args: Group*): Pattern = Pattern(sc.parts, args)
  }
  object Interpolation {
    trait Implicits {
      implicit val patternInterpolation: StringContext => Interpolation =
        new Interpolation(_)
    }
    object Implicits extends Implicits
  }

  private case class MapInstance(
    instance: String,
    range: Range,
    f: String => String = identity,
    children: Seq[MapInstance] = Seq.empty
  ) {
    def containsInChildren(other: MapInstance): Boolean =
      children.exists(_.contains(other))
    def contains(other: MapInstance): Boolean =
      range.contains(other.range.start) || containsInChildren(other)
    def add(child: MapInstance): MapInstance =
      if (containsInChildren(child)) copy(children = children.map(_.add(child)))
      else copy(children = child +: children)
    override def toString(): String =
      if (range.isEmpty) f("")
      else if (children.isEmpty) f(instance.substring(range.start, range.end))
      else {
        val start = MapInstance(instance, range.start until range.start)
        val end = MapInstance(instance, range.end until range.end)
        val sorted = (start +: end +: children).sortBy(_.range.start)
        val filled = sorted.sliding(2, 1).flatMap { case Seq(a, b) =>
          Seq(a, MapInstance(instance, a.range.end until b.range.start))
        }.toSeq.tail
        f(filled.mkString)
      }
  }

  implicit class RegexOps(val regex: Regex) extends AnyVal {
    /** Returns a string in which matched portions in `instance` are
      * mapped by `f`.
      *
      * `f` is a map from a group index to a map of matched portions.
      */
    def mapGroupsIn(instance: String)(f: Int => String => String): String =
      regex.replaceAllIn(instance, { m =>
        val source = m.source.toString
        val whole = MapInstance(source, 0 until source.length)
        m.subgroups.zipWithIndex.foldLeft(whole) { (r, g) =>
          val i = g._2 + 1
          val start = m.start(i)
          if (start >= 0) r.add(MapInstance(source, start until m.end(i), f(i)))
          else r
        }.toString
      })
  }
}
