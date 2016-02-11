package com.github.tarao.namedcap

import scala.collection.immutable.{Map => ImmutableMap}
import scala.collection.Map

object MultiMap {
  def apply(): MultiMap = new MultiMap(Map.empty)
  def apply[S <: Seq[String]](m: Map[String, S]): MultiMap = new MultiMap(m)
  val empty = MultiMap()
}

class MultiMap(m: Map[String, Seq[String]])
    extends ImmutableMap[String, String] {
  def get(key: String): Option[String] = m.get(key).flatMap(_.headOption)
  def getAll(key: String): Seq[String] = m.getOrElse(key, Seq.empty)
  def +[V >: String](kv: (String, V)): MultiMap = {
    val (key, value) = kv
    val list = m.getOrElse(key, Vector.empty) :+ value.asInstanceOf[String]
    new MultiMap(m + (key -> list))
  }
  def -(key: String): MultiMap = new MultiMap(m - key)
  def iterator: Iterator[(String, String)] =
    m.iterator.flatMap(kv => kv._2.headOption.map(kv._1 -> _))
  override def default(key: String): String =
    m.default(key).headOption.getOrElse {
      throw new NoSuchElementException("key not found: " + key)
    }
}
