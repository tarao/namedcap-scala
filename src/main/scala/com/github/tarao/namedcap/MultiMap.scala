package com.github.tarao.namedcap

import scala.collection.immutable.{Map => ImmutableMap}
import scala.collection.{Map, Seq}

object MultiMap {
  def apply(): MultiMap = new MultiMap(Map.empty)
  def apply[S <: Seq[String]](m: Map[String, S]): MultiMap = new MultiMap(m)
  val empty = MultiMap()
}

/** A map that can store multiple values with the same key.
  *
  * It returns multiple values by `getAll()` method.  All the other
  * methods are the same as single-valued maps; `apply()` or `get()`
  * returns the first value.
  */
class MultiMap(m: Map[String, Seq[String]])
    extends ImmutableMap[String, String] {
  def get(key: String): Option[String] = m.get(key).flatMap(_.headOption)
  def getAll(key: String): Seq[String] = m.getOrElse(key, Seq.empty)
  def iterator: Iterator[(String, String)] =
    m.iterator.flatMap(kv => kv._2.headOption.map(kv._1 -> _))
  def removed(key: String): MultiMap = new MultiMap(m - key)
  def updated[V1 >: String](key: String, value: V1): MultiMap = {
    val list = m.getOrElse(key, Seq.empty) :+ value.asInstanceOf[String]
    new MultiMap(m + (key -> list))
  }
  override def +[V >: String](kv: (String, V)): MultiMap = {
    val (key, value) = kv
    updated(key, value)
  }
  override def default(key: String): String =
    m.default(key).headOption.getOrElse {
      throw new NoSuchElementException("key not found: " + key)
    }
}
