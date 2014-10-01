package org.kframework.kast.outer

trait ModuleToString {
  self: Module =>
  override def toString = "module " + name + "\n" + sentences.toList.sortBy(_.toString).reverse.mkString("\n\n") + "\n\nendmodule"
}

trait DefinitionToString {
  self: Definition =>
  override def toString = modules.mkString("\n\n\n")
}

trait RuleToString {
  self: Rule =>
  override def toString = "  rule " + body + (
    if (attributes.isEmpty) "" else attributes)
}

trait ConfigurationToString {
  self: Configuration =>
  override def toString = "  configuration " + contents
}

trait SyntaxToString {
  self: Syntax =>
  override def toString = "  syntax " + sort + " ::= " + blocks.mkString("\n")
}

trait RegularProductionToString {
  self: RegularProduction =>
  override def toString = "" + items.mkString(" ") + (if (attributes.isEmpty) "" else " " + attributes)
}

trait BlockToString {
  self: Block =>
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

trait TerminalToString {
  self: Terminal =>
  override def toString = "\"" + value + "\""
}