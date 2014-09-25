package org.kframework.kast

import org.junit.Test
import org.kframework.parser.utils.QuickParser
import collection.JavaConversions._
import org.kframework._
import org.junit.Assert
import org.kframework.kil.Module
import org.kframework.parser.outer.Outer
import org.kframework.kil.Sources

class TestParser {
  import outer._

  val uglyBigString = """
// Copyright (c) 2014 K Team. All Rights Reserved.
require "modules/k-parser.k"
module K-SORT
  syntax MetaK
  syntax KString ::= Token{"dummy"} [onlyLabel, noAutoReject, regex("[\\\"]([^\\\"\\n\\r\\\\]|[\\\\][nrtf\\\"\\\\])*[\\\"]")]
  syntax KSort   ::= Token{"dummy"} [onlyLabel, noAutoReject, regex((?<![A-Za-z0-9])#?[A-Z][a-zA-Z0-9]*)]
endmodule


module OUTER
  imports K-SORT

  // Top level module stuff
  syntax KDefinition   ::= KRequireList KModuleList [klabel('KDef)]
  syntax KRequireList  ::= List{KRequire,""}        [klabel('ReqList)]
  syntax KRequire      ::= "require" KString
  syntax KModuleList   ::= List{KModule,""}
  syntax KModule       ::= "module" KModuleName KImportList KSentenceList "endmodule"
  syntax KImportList   ::= List{KImport,""}
  syntax KImport       ::= "imports" KModuleName
  syntax KSentenceList ::= List{Sentence,""}

  syntax KModuleName   ::= Token{"dummy"} [onlyLabel, noAutoReject, regex(#?[a-zA-Z](\-?[a-zA-Z0-9])*)]

  syntax Sentence ::= KSyntacticSentence
                    | KSemanticSentence

  syntax KSyntacticSentence ::= "syntax" KSort "::=" KPriorityBlockList
                              | "syntax" KSort
                              | "syntax" KSort KAttributes
  syntax KPriorityBlockList ::= NeList{KPriorityBlock,">"}
  syntax KPriorityBlock ::=             KProductionList
                         | "left:"      KProductionList
                         | "right:"     KProductionList
                         | "non-assoc:" KProductionList
  syntax KProductionList   ::= NeList{KProduction,"|"}
  syntax KProduction       ::= KSimpleProduction
                             | KSimpleProduction KAttributes
  syntax KSimpleProduction ::= NeList{KProductionItem,""}
                             | KTag "(" KSortList ")"
  syntax KSortList ::= List{KSort,","}
  syntax KProductionItem ::= KSort                            // non-terminal
                           | KString                          // terminals
                           | "Token{" KString "}"             // token
                           |   "List{" KSort "," KString "}"  // lists
                           | "NeList{" KSort "," KString "}"  // nelists
                           // maybe more

  syntax KSemanticSentence ::= "configuration" MetaK
                             | "configuration" MetaK KAttributes [prefer]
                             | "context"       MetaK
                             | "context"       MetaK                  KAttributes [prefer]
                             | "context"       MetaK "requires" MetaK
                             | "context"       MetaK "requires" MetaK KAttributes [prefer]
                             | "rule"          MetaK
                             | "rule"          MetaK                                   KAttributes [prefer]
                             | "rule"          MetaK "requires" MetaK
                             | "rule"          MetaK "requires" MetaK                  KAttributes [prefer]
                             | "rule"          MetaK "ensures"  MetaK
                             | "rule"          MetaK "ensures"  MetaK                  KAttributes [prefer]
                             | "rule"          MetaK "requires" MetaK "ensures"  MetaK
                             | "rule"          MetaK "requires" MetaK "ensures"  MetaK KAttributes [prefer]

  syntax KAttributes ::= "[" KAttributeList "]" [onlyLabel]
  syntax KAttributeList ::= List{KAttribute,","}

  syntax KAttribute ::= KTag
                      | KTag "(" KTagContent ")" // To consider: maybe allow KItem in here?
                      | KTag "(" KString     ")"
            
  syntax KTagContent ::= List{KTagC,""} [token] // anything with balanced parenthesis. Will have to get original string.
  syntax KTagC       ::= Token{"dummy"} [onlyLabel, noAutoReject, regex("[^\\n\\r\\(\\)\\\"]+")]
  syntax KTagC       ::= "(" KTagContent ")"


  syntax KTag    ::= Token{"dummy"} [onlyLabel, noAutoReject, regex([a-z][A-Za-z\-0-9]*)]
  syntax KTagList ::= List{KTag,","}

endmodule


module BUBBLE
  //syntax Bubble2 ::= Token{"dummy"} [onlyLabel, noAutoReject, regex(((?!rule|endmodule|syntax|configuration|context)(\S+)|\s+)+)]
                  //| Token{"dummy"} [onlyLabel, noAutoReject, regex([\`]{1}([^\`])+[\`]{1})]
                  //| Token{"dummy"} [onlyLabel, noAutoReject, regex([\`]{2}([^\`]|[\`][^\`])+[\`]{2})]
                  //| Token{"dummy"} [onlyLabel, noAutoReject, regex([\`]{3}([^\`]|[\`][^\`]|[\`][\`][^\`])+[\`]{3})]
                  // can add more if we want to
  syntax BubbleItem ::= Token{"dummy"} [onlyLabel, noAutoReject, regex(\S+)]
  syntax BubbleItem ::= "rule"          [reject]
                      | "syntax"        [reject]
                      | "configuration" [reject]
                      | "context"       [reject]
                      | "requires"      [reject]
                      | "ensures"       [reject]
                      | "endmodule"     [reject]
  syntax BubbleList ::= List{BubbleItem,""} [token]
  syntax Bubble ::= BubbleList [klabel('bubbleWrap)]
endmodule


module OUTER-WITH-BUBBLES
  imports OUTER
  imports BUBBLE
  syntax MetaK ::= Bubble
endmodule


module KAST
  imports K-SORT
  syntax MetaKLabel    ::= Token{"dummy"} [onlyLabel, noAutoReject, regex(`[^`\s]+`)]
                     //| "`" MetaKLabel "`"  [bracket]
  syntax KConstant ::= "#token"  "("KString "," KString ")"
                     | "#klabel" "(" MetaKLabel ")"
  syntax MetaKItem ::= KConstant
                     | MetaKLabel "(" MetaKList ")"
  syntax MetaKList     ::= NeList{MetaK,","}
                     | ".::MetaKList"
  // we need to say something like List{K}{","}{"."}
  syntax MetaK         ::= NeList{MetaKItem,"~>"}
                     | ".::MetaK"
  // we need to say List{MetaKItem}{"~>"}{"."}

  syntax KVariable ::= Token{"dummy"} [onlyLabel, noAutoReject, regex((?<![A-Za-z0-9\_])(\$|\!|\?)?([A-Z][A-Za-z0-9']*|_))]
  syntax MetaKItem ::= KVariable
                     | MetaKItem ":" KSort  // KItem must obey its sort at runtime
                                     // sorting arbitrary KItems; Sort can also be
                                     // K, KItem, KToken, but not KList
  syntax MetaK ::= MetaK "=>" MetaK       [onlyLabel]
                 | MetaK "requires" MetaK [onlyLabel]
                 | MetaK "ensures"  MetaK [onlyLabel]
                 | "`" MetaK "`"  [bracket]
endmodule


module TEST
  imports OUTER
  imports KAST
  imports K-PARSER
  imports OUTER-WITH-BUBBLES
  
  configuration <k> $PGM:KAny </k>
//  syntax KAny ::= Token{~[a]*} [onlyLabel, noAutoReject, regex((?s).*)]
  
//  rule S:KAny         => #parseInModule(#tokenToString(S), "KDefinition", "OUTER-WITH-BUBBLES")
//  rule 'bubbleWrap(S) => #parseInModule(#tokenToString(S), "MetaK",       "KAST")                [anywhere]

endmodule
"""

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

  @Test def testUglyBigStringOfBeautifulBigDefinition() {
    val module = constructFromString(uglyBigString)
    assertParseWithModule(uglyBigString, kil.Sort.of("KDefinition"), """
 
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