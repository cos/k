package org.kframework.kast.inner

import org.junit.Test
import org.junit.runner.RunWith
import org.mockito.runners.MockitoJUnitRunner
import collection.JavaConversions._
import org.kframework._

@RunWith(classOf[MockitoJUnitRunner])
class TestKILtoKAST {

  import backend.java.kil
  import org.kframework.kast.outer

  @Test
  def testTest() {
    val play = """
    module TEST
      syntax Exp ::= "a" | "b"

      rule a => b

      configuration <k> $PGM </k>
    endmodule
    """

    val kil = TextToBackendKIL.parseDefinition(play)

    println(kil)
  }
}