package org.kframework.kast.outer

import org.kframework.kast

object Definition {
  def apply(modules: Module*): Definition = Definition(modules.toSet)
}

case class Definition(modules: Set[Module]) {
  override def toString = modules.mkString("\n\n\n")
}

case class Module(
  name: String,
  sentences: Set[Sentence]) {
  override def toString = "module " + name + "\n" + sentences.toList.sortBy(_.toString).reverse.mkString("\n\n") + "\n\nendmodule"
}

trait Sentence

case class Rule(
  label: String,
  body: kast.Term,
  attributes: kast.Attributes) extends Sentence {
  override def toString = "  rule " + body + (
    if (attributes.isEmpty) "" else attributes)
}

case class Configuration(contents: kast.Term) extends Sentence {
  override def toString = "  configuration " + contents
}

case class ModuleComment(comment: String) extends Sentence {
  
}

case class Import(what: String) extends Sentence

object Associativity extends Enumeration {
  val Left, Right, NonAssoc, Unspecified = Value
}

case class Syntax(sort: kast.Sort, blocks: Seq[Block]) extends Sentence {
  override def toString = "  syntax " + sort + " ::= " + blocks.mkString("\n")
}

case class SyntaxPriority(productions: Set[SyntaxPriorityBlock]) extends Sentence {
} 

case class SyntaxPriorityBlock(productions: Seq[kast.Label]) {
  
}

trait ProductionItem

trait Production {
  val attributes: kast.Attributes
  def getKLabel: String
}

case class NormalProduction(items: Seq[ProductionItem], attributes: kast.Attributes) extends Production {
  override def toString = "" + items.mkString(" ") + (if (attributes.isEmpty) "" else " " + attributes)

  def getKLabel = "'" + (items map {
    case _: NonTerminal => "_"
    case Terminal(value) => value
  } mkString)
}

case class UserList(sort: kast.Sort, separator: String, attributes: kast.Attributes) extends Production {
  def getKLabel = "'_" + separator + "_"
}

case class NonTerminal(name: String, sort: kast.Sort) extends ProductionItem
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