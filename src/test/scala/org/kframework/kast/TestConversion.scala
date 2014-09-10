package org.kframework.kast

import org.junit.Test
import org.junit.runner.RunWith
import org.mockito.runners.MockitoJUnitRunner
import collection.JavaConversions._
import org.kframework._

@RunWith(classOf[MockitoJUnitRunner])
class TestConversion extends AbstractConversionTest {

  import backend.java.kil
  import org.kframework.kast

  @Test def testEmptyModule() {
    assertFullConversion("", "", "")
  }
//
//  @Test def testSimpleSyntax() {
//    assertKILtoKAST("""
//      syntax Exp ::= "a"
//      """, """
//  syntax KLabel ::= "'a" [arity(0)]
//    """)
//  }
//
//  @Test def testSimpleRule() {
//    assertKILtoKAST("""
//      syntax Exp ::= "a" | "b"
//      rule a => b
//      """, """
//  syntax KLabel ::= "'b" [arity(0)]
//
//  syntax KLabel ::= "'a" [arity(0)]
//
//  rule <generatedTop>Bag(<k>'a:KItem~>GeneratedFreshVar0:K</k>, GeneratedFreshVar1:Bag):Bag[bag]</generatedTop> => <generatedTop>Bag(<k>'b:KItem~>GeneratedFreshVar0:K</k>, GeneratedFreshVar1:Bag):Bag[bag]</generatedTop>?>rule<?[computational(), lookups([]), concreteDataStructureSize({})]
//    """)
//  }
//
//  @Test
//  def testSimpleModule() {
//
//    assertKILtoKAST(
//      """
//      syntax Exp ::= "a" | "b"
//
//      rule a => b
//
//      configuration <k> $PGM </k>
//""", """
//  syntax KLabel ::= "'b" [arity(0)]
//
//  syntax KLabel ::= "'a" [arity(0)]
//
//  rule <generatedTop>Bag(<k>'a:KItem~>GeneratedFreshVar2:K</k>, GeneratedFreshVar3:Bag):Bag[bag]</generatedTop> => <generatedTop>Bag(<k>'b:KItem~>GeneratedFreshVar2:K</k>, GeneratedFreshVar3:Bag):Bag[bag]</generatedTop>?>rule<?[computational(), lookups([]), concreteDataStructureSize({})]
//""")
//  }
}