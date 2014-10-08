package org.kframework.kore.outer

import org.kframework.kore
import org.kframework.kore.Sort
import scala.util.matching.Regex
import org.kframework.kore.Attributes

trait ParserPiece

case class Definition(requires: Set[Require], modules: Set[Module])
  extends DefinitionToString with ParserPiece

case class Require(file: java.io.File)

object Module {
  def apply(name: String, att: Attributes, sentences: Set[Sentence]): Module = 
    Module(name, sentences, att)
}

case class Module(name: String, sentences: Set[Sentence], att: Attributes = Attributes())
  extends ModuleToString with ParserPiece {
  val labelsToProductions: Map[kore.KLabel, Set[SyntaxProduction]] = {
    ???
  }
}
// hooked but different from core, Import is a sentence here

trait Sentence { // marker
  val attributes: Attributes
}

case class Rule(
  label: String,
  body: kore.K,
  attributes: Attributes) extends Sentence
  with RuleToString

case class Configuration(contents: kore.K, attributes: Attributes = Attributes()) extends Sentence // hooked
  with ConfigurationToString

case class ModuleComment(comment: String, attributes: Attributes = Attributes()) extends Sentence

case class Import(what: String, attributes: Attributes = Attributes()) extends Sentence // hooked

case class SyntaxPriority(higher: String, lower: String, attributes: Attributes = Attributes()) extends Sentence with ParserPiece

//object Associativity extends Enumeration {
//  val Left, Right, NonAssoc, Unspecified = Value
//}

case class SyntaxSort(sort: Sort, attributes: Attributes = Attributes()) extends Sentence with ParserPiece

case class SyntaxProduction(sort: Sort, items: Seq[ProductionItem], attributes: Attributes = Attributes()) extends Sentence with ParserPiece // hooked but problematic, see kast-core.k 
  with SyntaxProductionToString {
  def klabel = "'" + (items map {
    case _: NonTerminal => "_"
    case Terminal(value) => value
  } mkString)
}

trait ProductionItem // marker

//trait Production {
//  val attributes: kast.Attributes
//  def klabel: String
//}

//case class UserList(sort: Sort, separator: String, attributes: kast.Attributes) extends Production { // different from kast core!
//  def klabel = "'_" + separator + "_"
//}

case class NonTerminal(sort: Sort) extends ProductionItem // hooked but it seems we have an extra "name" here


case class RegexTerminal(regex: String) extends ProductionItem // the equivalent for this is actually a KProduction in kore kast
case class Terminal(value: String) extends ProductionItem // hooked
  with TerminalToString
  
