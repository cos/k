package org.kframework.kast.inner

import org.junit.Test
import org.junit.runner.RunWith
import org.mockito.runners.MockitoJUnitRunner
import collection.JavaConversions._
import org.kframework._

@RunWith(classOf[MockitoJUnitRunner])
class TestKILtoKAST extends AssertKILtoKAST {

  import backend.java.kil
  import org.kframework.kast.inner

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
  syntax KLabel ::= "'a" [ arity(0)]

  syntax KLabel ::= "'b" [ arity(0)]

  rule <generatedTop>Bag(<k>'a:KItem⤳GeneratedFreshVar0:K</k>, GeneratedFreshVar1:Bag):Bag</generatedTop> => <generatedTop>Bag(<k>'b:KItem⤳GeneratedFreshVar0:K</k>, GeneratedFreshVar1:Bag):Bag</generatedTop>?>rule<? [ computational()]
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
syntax KLabel ::= "'a" [ arity(0)]

  configuration <generatedTop><k>$PGM:Bag</k></generatedTop>

  syntax CellLabel ::= "k"

  syntax KLabel ::= "'b" [ arity(0)]

  rule <generatedTop>Bag(<k>'a:KItem⤳GeneratedFreshVar2:K</k>, GeneratedFreshVar3:Bag):Bag</generatedTop> => <generatedTop>Bag(<k>'b:KItem⤳GeneratedFreshVar2:K</k>, GeneratedFreshVar3:Bag):Bag</generatedTop>?>rule<? [ computational()]

  syntax CellLabel ::= "generatedTop"

  syntax KLabel ::= "'" [ arity(0)]
""")
  }
}