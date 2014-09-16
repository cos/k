package org.kframework.kast

import scala.reflect.ClassTag

object Attributes {
  def apply(): Attributes = Attributes(Map())
}

trait Key[T] {
  val key: Symbol
}

case class Flag(key: Symbol) extends Key[Boolean]

case class Attributes(a: Map[Symbol, Any]) {
  import Attributes._

  override def toString = "[" + a.map({
    case (k, v) =>
      if (v == true) k.name else k.name + "(" + v + ")"
  }).mkString(", ") + "]"

  def isEmpty = a.isEmpty

  def +(flag: Key[Boolean]) = Attributes(a + (flag.key -> true))
  def +(t: (Key[_], Any)) = Attributes(a + (t._1.key -> t._2))

  def ++(other: Attributes) = Attributes(a ++ other.a)

  def apply[V](key: Key[V]): V = key match {
    case Flag(key) => a.contains(key)
    case _ => a(key.key).asInstanceOf[V]
  }
  def get[V](key: Key[V]): Option[V] = a.get(key.key).asInstanceOf[Option[V]]
}

trait NoAttributes {
  lazy val attributes = Attributes()
}
