package org.kframework.kore.outer

import org.kframework.kast
import org.kframework.kast.Attributes
import scala.util.matching.Regex

case class Definition(requires: Set[Require], modules: Set[Module])
  extends DefinitionToString

case class Require(file: java.io.File) 

case class Module(name: String, att: Attributes = Attributes(), sentences: Set[Sentence])
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

case class SyntaxPriority(higher: String, lower: String, attributes: Attributes = Attributes()) extends Sentence

//object Associativity extends Enumeration {
//  val Left, Right, NonAssoc, Unspecified = Value
//}

case class SyntaxSort(sort: kast.Sort, attributes: Attributes = Attributes()) extends Sentence

case class SyntaxProduction(sort: kast.Sort, items: Seq[ProductionItem], attributes: Attributes = Attributes()) extends Sentence // hooked but problematic, see kast-core.k 
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


//case class UserList(sort: kast.Sort, separator: String, attributes: kast.Attributes) extends Production { // different from kast core!
//  def klabel = "'_" + separator + "_"
//}

case class NonTerminal(sort: kast.Sort) extends ProductionItem // hooked but it seems we have an extra "name" here
case class RegexTerminal(regex: Regex) extends ProductionItem // the equivalent for this is actually a KProduction in kore kast
case class Terminal(value: String) extends ProductionItem // hooked
  with TerminalToString
  
case class KSort(name: String)
