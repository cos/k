package org.kframework.kast

import scala.reflect.ClassTag

trait Key[T] {
  val key: Symbol
}

case class Flag(key: Symbol) extends Key[Boolean]
case class CurrentStyleKey[T](theKey: org.kframework.kil.Attribute.Key[T]) extends Key[T] {
  val key = Symbol(theKey.toString())
}

object UglyRecorder {
  val ugly: collection.mutable.Map[Symbol, org.kframework.kil.Attribute.Key[Any]] = collection.mutable.Map()
}

//object Attributes {
//  def apply(a: Map[Key[_], _]): Attributes = new Attributes(a map { case (k, v) => (k.key, v) }, true)
//}

case class Attributes(a: Map[Symbol, Any] = Map()) {

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

  def foreach(f: ((Symbol, Any)) => Unit) {
    a foreach f
  }
}

trait NoAttributes {
  lazy val attributes = Attributes()
}
