package org.kframework.kast

import shapeless._

trait KLabel {
  val name: String
  def apply(klist: Seq[Term], attributes: Attributes): Term

  override def toString = name
}

trait Term {
  val klabel: KLabel
  val klist: scala.Seq[Term]
  val attributes: Attributes

  def copy(klist: Seq[Term] = klist, attributes: Attributes = attributes): Term = klabel(klist, attributes)
  
  override def toString = {
    val klistString = if (klist.isEmpty) "" else "(" + klist.toList.sortBy(_.toString).mkString(", ") + ")"

    val cleanAttributes = (attributes.a.filter({ case (k, _) => k != 'sort }))
    val attributesString = if (cleanAttributes.isEmpty) "" else ("[" + cleanAttributes.toString() + "]")

    val sortString = attributes.get('sort).map(":" + _).getOrElse("")

    klabel.name + klistString + sortString + attributesString
  }
}

object Attributes {
  val on = "on"
  def apply(): Attributes = Attributes(Map())
}

case class Attributes(a: Map[Symbol, String]) {
  import Attributes._

  override def toString = a.map({
    case (k, v) =>
      if (v == on)
        k.name
      else
        k.name + "(" + v + ")"
  }).mkString(", ")

  def isEmpty = a.isEmpty

  def +(s: Symbol) = Attributes(a + (s -> on))
  def +(t: (Symbol, String)) = Attributes(a + t)

  def ++(other: Attributes) = Attributes(a ++ other.a)

  def apply(s: Symbol): String = a(s)
  def get(s: Symbol): Option[String] = a.get(s)
}

class WrongNumberOfChildrenException(actual: Int, expected: Int) extends RuntimeException(s"Wrong number of children. Actual: $actual Expected:$expected")

trait Term0 extends Term {
  final val klist = Seq()
  if (klist.size != 0)
    throw new WrongNumberOfChildrenException(klist.size, 0)
}
trait Term1 extends Term {
  if (klist.size != 1)
    throw new WrongNumberOfChildrenException(klist.size, 1)
}
trait Term2 extends Term {
  if (klist.size != 2)
    throw new WrongNumberOfChildrenException(klist.size, 1)
}

trait KLabelTerm extends KLabel with Term {
  final val klabel = this
}

trait SingletonTerm extends Term {
  def apply(klist: Seq[Term], attributes: Attributes): Term = this
}

trait NoAttributes {
  lazy val attributes = Attributes()
}

trait TermConstructor[Context] {
  // apply assumentes the klist is clean, i.e., it has been constructed with this TermConstructor
  def apply(klabel: String, klist: Seq[Term], attributes: Attributes)(implicit context: Context): Term

  // could be optimized to not convert things that are already in the format we want
  def convert(termToConvert: Term)(implicit context: Context): Term = {
    import termToConvert._
    apply(klabel.toString, klist.toList map convert, attributes)
  }
}