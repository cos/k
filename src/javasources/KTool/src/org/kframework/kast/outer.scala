package org.kframework.kast

object Definition {
  def apply(modules: Module*): Definition = Definition(modules.toSet)
  type Attributes = Map[String, String]
}

object Attributes {
  val on = "on"
  def apply(): Attributes = Attributes(Map())
}

case class Attributes(a: Map[String, String]) {
  import Attributes._
  
  override def toString = a.map({
    case (k, v) =>
      if (v == "on")
        k
      else
        k + "(" + v + ")"
  }).mkString(", ")

  def isEmpty = a.isEmpty

  def +(s: String) = Attributes(a + (s -> on))
  def +(t: (String, String)) = Attributes(a + t)
}

import Definition._

case class AttributesDeclaration(attributes: Attributes) {
  override def toString =
    if (attributes.isEmpty)
      ""
    else
      " [ " + attributes + "]"
}

case class Definition(modules: Set[Module]) {
  override def toString = modules.mkString("\n\n\n")
}

case class Module(
  name: String,
  sentences: Set[Sentence]) {
  override def toString = "module " + name + "\n" + sentences.mkString("\n\n") + "\n\nendmodule"
}

trait Sentence

case class Rule(
  body: K,
  requires: Option[KItem],
  ensures: Option[KItem],
  attributes: AttributesDeclaration) extends Sentence {
  override def toString = "  rule " + body + "?>rule<?" + attributes
}

case class Configuration(contents: K) extends Sentence {
  override def toString = "  configuration " + contents
}

case class Import(what: String) extends Sentence

object Associativity extends Enumeration {
  val Left, Right, NonAssoc, Unspecified = Value
}

case class Syntax(sort: Sort, blocks: Seq[Block]) extends Sentence {
  override def toString = "  syntax " + sort + " ::= " + blocks.mkString("\n")
}

trait ProductionItem

trait Production {
  val attributes: AttributesDeclaration
}

case class NormalProduction(items: Seq[ProductionItem], attributes: AttributesDeclaration) extends Production {
  override def toString = "" + items.mkString(" ") + attributes
}

case class UserList(sort: Sort, separator: String, attributes: AttributesDeclaration) extends Production

case class NonTerminal(name: String, sort: Sort) extends ProductionItem
trait Regex
case class Lexical(regex: Regex) extends ProductionItem
case class Terminal(value: String) extends ProductionItem {
  override def toString = "\"" + value + "\""
}

case class Block(assoc: Associativity.Value, productions: Set[Production]) {
  override def toString = {
    import Associativity._
    val assocString = assoc match {
      case Unspecified => ""
      case Left => "left:\n"
      case Right => "right:\n"
      case NonAssoc => "non-assoc:\n"
    }
    assocString + productions.mkString("\n              |")
  }
}