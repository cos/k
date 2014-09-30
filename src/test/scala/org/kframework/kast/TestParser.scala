package org.kframework.kast

import org.junit.Test
import org.kframework.parser.utils.QuickParser
import collection.JavaConversions._
import org.kframework._
import org.junit.Assert
import org.kframework.kil.Module
import org.kframework.parser.outer.Outer
import org.kframework.kil.Sources
import java.io.File

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
      """#token("Exp","1")(.KList)""" + """ + #token("Exp","2")(.KList) """ * 2)
  }

  @Test def test10Plus() {
    val times = 10
    assertParse(
      "1" + "+2" * times,
      """#token("Exp","1")(.KList)""" + """ + #token("Exp","2")(.KList) """ * times)
  }

  def assertParse(program: String, expected: String) {
    val module = new kil.Module()
    val mi = productions map convert
    module.setItems(mi)
    assertParseWithModule(program, ExpSort, expected, module)
  }

  def assertParseWithModule(program: String, startSymbol: kil.Sort, expected: String, module: kil.ASTNode) {
    val term = QuickParser.parse(program, startSymbol, module)
    Assert.assertEquals(expected, term.toString())
  }

  @Test def testDirectly() {
    val module = constructFromString("""
module TEST
  syntax Exp ::= Token{[0-9]+} 
               | Exp "+" Exp [ left ]
endmodule
""")
    assertParseWithModule("1 + 2", kil.Sort.of("Exp"), """
#token("Exp","1")(.KList) + #token("Exp","2")(.KList) 
""", module)
  }

  @Test def testKORE() {
    val definition = scala.io.Source.fromFile("samples/kast/kore.k").mkString
    
    val module = constructFromString(definition)
    assertParseWithModule(definition, kil.Sort.of("KDefinition"), """
 
""", module)
  }

  def constructFromString(definitionString: String): kil.Definition = {
    val definition = new kil.Definition()
    val items = Outer.parse(Sources.generatedBy(classOf[TestParser]), definitionString, null)
    definition.setItems(items)
    definition
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
    exp.putAttribute("left", null)

    val priorityBlock = new kil.PriorityBlock(null, exp)

    val syntax = new kil.Syntax(new kil.NonTerminal(p.sort), priorityBlock)

    syntax
  }
}