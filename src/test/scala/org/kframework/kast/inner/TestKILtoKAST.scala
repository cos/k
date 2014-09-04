package org.kframework.kast.inner

import org.junit.Test
import org.junit.runner.RunWith
import org.mockito.runners.MockitoJUnitRunner
import collection.JavaConversions._
import org.kframework._
import org.kframework.kast.KILtoKAST

@RunWith(classOf[MockitoJUnitRunner])
class TestKILtoKAST {

  import backend.java.kil
  import org.kframework.kast.inner

  @Test
  def testTest() {
    val play = """
    module TEST
      syntax Exp ::= "a" | "b"

      rule a => b

      configuration <k> $PGM </k>
    endmodule
    """

    val (kil, _, _) = TextToBackendKIL.parseDefinition(play)

    val converted = KILtoKAST.convert(kil)

    println(kil)
    
    println(converted)
  }
}