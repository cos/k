package org.kframework.kast

import org.junit.Test
import org.junit.runner.RunWith
import org.mockito.runners.MockitoJUnitRunner
import collection.JavaConversions._
import org.kframework._

@RunWith(classOf[MockitoJUnitRunner])
class TestConversion extends AssertKILtoKAST {

  import backend.java.kil
  import org.kframework.kast

  @Test def testEmptyModule() {
    assertConversion("", "")
  }

  @Test def testSimpleSyntax() {
    assertConversion("""
      syntax Exp ::= "a"
      """, """
      syntax KLabel ::= "'a" [ arity(0)]
    """)
  }

  @Test def testSimpleRule() {
    assertConversion("""
      syntax Exp ::= "a" | "b"
      rule a => b
      """, """
  syntax KLabel ::= "'b" [ arity(0)]

  syntax KLabel ::= "'a" [ arity(0)]

  rule <generatedTop>Bag(<k>'a:KItem⤳GeneratedFreshVar0:K</k>, GeneratedFreshVar1:Bag):Bag[Map('bag -> on)]</generatedTop> => <generatedTop>Bag(<k>'b:KItem⤳GeneratedFreshVar0:K</k>, GeneratedFreshVar1:Bag):Bag[Map('bag -> on)]</generatedTop>?>rule<? [ computational()]
    """)
  }

  @Test
  def testSimpleModule() {

    assertConversion(
      """
      syntax Exp ::= "a" | "b"

      rule a => b

      configuration <k> $PGM </k>
""", """
  syntax KLabel ::= "'b" [ arity(0)]

  syntax KLabel ::= "'a" [ arity(0)]

  syntax KLabel ::= "'" [ arity(0)]

  syntax CellLabel ::= "k"

  syntax CellLabel ::= "generatedTop"

  rule <generatedTop>Bag(<k>'a:KItem⤳GeneratedFreshVar2:K</k>, GeneratedFreshVar3:Bag):Bag[Map('bag -> on)]</generatedTop> => <generatedTop>Bag(<k>'b:KItem⤳GeneratedFreshVar2:K</k>, GeneratedFreshVar3:Bag):Bag[Map('bag -> on)]</generatedTop>?>rule<? [ computational()]

  configuration <generatedTop><k>$PGM:Bag</k></generatedTop>
""")
  }
}