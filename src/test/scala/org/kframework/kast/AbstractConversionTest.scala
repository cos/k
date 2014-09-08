package org.kframework.kast

import org.kframework.kast.convertors.KILtoKAST
import org.junit.Assert._
import org.kframework.kast.convertors.KASTtoBackendKIL

class AbstractConversionTest extends JUnitTest {

  def moduleName = name.getMethodName().replace("name", "").toUpperCase

  def ensuredModule(s: String): String =
    if (!s.trim.startsWith("module"))
      "module " + moduleName + "\n" + s + "\nendmodule\n"
    else
      s

  def ensuredConfiguration(s: String): String = {
    val configurationText = "\n\n  configuration <k> $PGM </k>"
    if (s.contains("configuration"))
      s
    else if (s.contains("endmodule"))
      s.replace("endmodule", "") + configurationText + "\n\nendmodule"
    else
      s + configurationText
  }

  val junk = List(
    "configuration <generatedTop><k>$PGM:Bag</k></generatedTop>",
    "syntax KLabel ::= \"'\" [ arity(0)]",
    "syntax CellLabel ::= \"generatedTop\"",
    "syntax CellLabel ::= \"k\"") map ("\n  " + _ + "\n")

  def assertKILtoKAST(kilText: String, kastText: String) = {

    val fullKilModule = ensuredConfiguration(ensuredModule(kilText))

    val compiler = TestingCompiler(fullKilModule, moduleName)

    val converted = KILtoKAST(compiler.kompiledKIL)

    val actual = if (kastText.contains("module"))
      converted.toString
    else {
      val noModuleWrapper = converted.toString.replace("module " + moduleName, "").replace("endmodule", "")

      "  " + (junk.fold(noModuleWrapper) { _.replace(_, "") }).trim
    }

    val expected =
      if (kastText.contains("module"))
        kastText
      else
        "  " + kastText.trim

    println(actual)
    assertEquals(expected, actual)

    converted
  }

  def assertKASTtoBackendKIL(kast: Definition, backendKILText: String) {
    ???
  }

  def assertFullConversion(kilText: String, kastText: String, backendKILText: String) {
    val kast = assertKILtoKAST(kilText, kastText)
    assertKASTtoBackendKIL(kast, backendKILText)
  }

  def assertConversion(kilText: String) {
    val fullKilModule = ensuredConfiguration(ensuredModule(kilText))
    val compiler = TestingCompiler(fullKilModule, moduleName)
    val kast = KILtoKAST(compiler.kompiledKIL)
    val convertedBackendKIL = KASTtoBackendKIL(kast)
    assertEquals(compiler.javaBackendKIL.toString(), convertedBackendKIL.toString())
  }
}