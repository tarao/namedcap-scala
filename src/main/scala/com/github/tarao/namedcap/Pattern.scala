package com.github.tarao.namedcap

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.util.matching.Regex

/** A class to describe a pattern. */
case class Pattern(patterns: Seq[String], groups: Seq[Group]) {
  def groupNames(): Seq[String] = groups.flatMap(_.names)
  def namedGroups(): Seq[(String, Group)] = groups.flatMap(_.namedGroups)

  def allGroupNames(): Seq[String] = groups.flatMap(_.allNames)
  def allNamedGroups(): Seq[(String, Group)] = groups.flatMap(_.allNamedGroups)

  def mapGroups(f: Group => String): String =
    patterns.zipAll(groups.map(_.mapGroups(f)), "", "").map { zipped =>
      zipped._1 + zipped._2
    }.mkString

  def mapGroupsIn(instance: String)(f: (Group, String) => String): String = {
    import Pattern.RegexOps
    val indexToGroup: Map[Int, Group] =
      allNamedGroups.zipWithIndex.map { case ((_, g), i) =>
        (i + 1) -> g
      }(scala.collection.breakOut)
    r.mapGroupsIn(instance)(Function.uncurried(f.curried compose indexToGroup))
  }

  def r: Regex = mapGroups(_.r.toString).r

  def apply(instance: String): MultiMap = {
    r.findFirstMatchIn(instance).map { m =>
      val map = new HashMap[String, ListBuffer[String]]
      (allNamedGroups, m.subgroups).zipped.foreach { case ((g, _), s) =>
        Option(s).foreach(s => map.getOrElseUpdate(g, new ListBuffer) += s)
      }
      MultiMap(map)
    }.getOrElse(MultiMap.empty)
  }

  def +(other: Group): Pattern =
    Pattern(Seq("", ""), Seq(this, other))
  def +(other: String): Pattern = Pattern(Seq("", other), Seq(this))

  override def toString(): String = mapGroups {
    case UnnamedGroup(p) => p.toString
    case g => s"$${${g.name}}"
  }
}
object Pattern extends Implicits {
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
      * `f` is a map from a group index and a matched portion to a
      * replacement.
      */
    def mapGroupsIn(instance: String)(f: (Int, String) => String): String =
      regex.replaceAllIn(instance, { m =>
        val source = m.source.toString
        val whole = MapInstance(source, 0 until source.length)
        m.subgroups.zipWithIndex.foldLeft(whole) { (r, g) =>
          val i = g._2 + 1
          val start = m.start(i)
          if (start >= 0)
            r.add(MapInstance(source, start until m.end(i), f.curried(i)))
          else r
        }.toString
      })
  }
}
