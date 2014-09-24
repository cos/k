package org.kframework.kast

import org.junit.Test
import org.kframework.parser.utils.QuickParser
import collection.JavaConversions._
import org.kframework._
import org.junit.Assert
import org.kframework.kil.Module

class TestParser {
  import outer._

  object Production {
    def apply(sort: Sort)(items: ProductionItem*): Production = Production(sort, items, Attributes())
  }

  case class Production(sort: Sort, items: Seq[ProductionItem], att: Attributes = Attributes())

  implicit def sortToNonTerminal(s: Sort) = NonTerminal(s)

  val ExpSort = Sort("Exp")
  val Int = Production(ExpSort)(Lexical("[0-9]+".r))
  val Plus = Production(ExpSort)(ExpSort, Terminal("+"), ExpSort)

  val productions = List(Int, Plus)

  @Test def test1Terminal() {
    assertParse(
      "1",
      """#token("Exp","1")(.KList)""")
  }

  @Test def testInt() {
    assertParse(
      "2",
      """#token("Exp","2")(.KList)""")
  }

  @Test def testPlus() {
    assertParse(
      "1 + 2",
      """#token("Exp","1")(.KList) + #token("Exp","2")(.KList) """)
  }
  
  @Test def test2Plus() {
    assertParse(
      "1" + "+2" * 2,
      """#token("Exp","1")(.KList)""" +  """ + #token("Exp","2")(.KList) """ * 2)
  }
  
  @Test def test10Plus() {
    assertParse(
      "1" + "+2" * 10,
      """#token("Exp","1")(.KList)""" +  """ + #token("Exp","2")(.KList) """ * 10)
  }

  def assertParse(program: String, expected: String) {
    val module = new kil.Module()
    val mi = productions map convert
    module.setItems(mi)

    val term = QuickParser.parse(program, ExpSort, module)
    Assert.assertEquals(expected, term.toString())
  }

  implicit def convert(p: outer.ProductionItem): kil.ProductionItem = p match {
    case outer.Terminal(s) => new kil.Terminal(s)
    case outer.NonTerminal(sort) => new kil.NonTerminal(sort)
    case outer.Lexical(regex) =>
      val lexical = new kil.Lexical(null, null)
      lexical.setLexicalRule(regex.toString())
      lexical
  }

  implicit def convert(s: Sort): kil.Sort = kil.Sort.of(s.name)

  implicit def convert(p: Production): kil.Syntax = {
    val productionItems = p.items map convert
    val exp = new kil.Production(new kil.NonTerminal(p.sort), productionItems)

    val priorityBlock = new kil.PriorityBlock(null, exp)

    val syntax = new kil.Syntax(new kil.NonTerminal(p.sort), priorityBlock)

    syntax
  }
}