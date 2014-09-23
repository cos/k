package org.kframework.kast.outer

import org.kframework.kast
import org.kframework.kast.Attributes

object Definition {
  def apply(requires: Require, modules: Module*): Definition = Definition(Set[Require](), modules.toSet)
}

case class Definition(requires: Set[Require], modules: Set[Module]) // hooked
  extends DefinitionToString

case class Require(file: java.io.File) // hooked

case class Module(name: String, sentences: Set[Sentence])
  extends ModuleToString
// hooked but different from core, Import is a sentence here

trait Sentence { // marker
  val attributes: kast.Attributes
} 

case class Rule(
  label: String,
  body: kast.Term,
  attributes: Attributes) extends Sentence
  with RuleToString

case class Configuration(contents: kast.Term, attributes: Attributes = Attributes()) extends Sentence // hooked
  with ConfigurationToString

case class ModuleComment(comment: String, attributes: Attributes = Attributes()) extends Sentence

case class Import(what: String, attributes: Attributes = Attributes()) extends Sentence // hooked

case class SyntaxPriority(productions: Set[SyntaxPriorityBlock], attributes: Attributes = Attributes()) extends Sentence
case class SyntaxPriorityBlock(productions: Seq[kast.Label])

object Associativity extends Enumeration {
  val Left, Right, NonAssoc, Unspecified = Value
}

case class Syntax(sort: kast.Sort, blocks: Seq[Block] = Seq(), attributes: Attributes = Attributes()) extends Sentence // hooked but problematic, see kast-core.k 
  with SyntaxToString

case class Block(assoc: Associativity.Value, productions: Set[Production])
  extends BlockToString

trait ProductionItem // marker

trait Production {
  val attributes: kast.Attributes
  def klabel: String
}

case class RegularProduction(items: Seq[ProductionItem], attributes: kast.Attributes) extends Production // hooked but lacking attributes in kast core
  with RegularProductionToString {
  def klabel = "'" + (items map {
    case _: NonTerminal => "_"
    case Terminal(value) => value
  } mkString)
}

case class UserList(sort: kast.Sort, separator: String, attributes: kast.Attributes) extends Production { // different from kast core!
  def klabel = "'_" + separator + "_"
}

case class NonTerminal(name: String, sort: kast.Sort) extends ProductionItem // hooked but it seems we have an extra "name" here
trait Regex
case class Lexical(regex: Regex) extends ProductionItem // the equivalent for this is actually a KProduction in kore kast
case class Terminal(value: String) extends ProductionItem // hooked
  with TerminalToString
