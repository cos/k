package org.kframework.kast

import org.kframework.kast.convertors.KILtoKAST
import org.junit.Assert._

trait AssertKILtoKAST extends JUnitTest {

  def moduleName = name.getMethodName().replace("name", "").toUpperCase

  def makeModule(s: String) = "module " + moduleName + "\n" + s + "\nendmodule\n"

  def assertConversion(kilText: String, kastText: String) = {
    val fullModuleAssertion = kilText.trim.startsWith("module")

    val configurationText = "\n\n  configuration <k> $PGM </k>"

    val junk = List(
      "configuration <generatedTop><k>$PGM:Bag</k></generatedTop>",
      "syntax KLabel ::= \"'\" [ arity(0)]",
      "syntax CellLabel ::= \"generatedTop\"",
      "syntax CellLabel ::= \"k\"") map ("\n  " + _ + "\n")

    val (fullKilModule, fullKastText) =
      if (fullModuleAssertion) {
        (kilText, kastText)
      } else {
        val fullKilModule = makeModule(if (kilText.contains("configuration"))
          kilText
        else
          kilText + configurationText)

        (fullKilModule, makeModule(kastText))
      }

    ///
    val compiler = TestingCompiler(fullKilModule, moduleName)
    println(compiler.parsedKIL)
    val converted = KILtoKAST(compiler.kompiledKIL)
    ///

    val actual =
      if (fullModuleAssertion)
        converted.toString.trim
      else {
        val noModuleWrapper = converted.toString.replace("module " + moduleName, "").replace("endmodule", "")

        "  " + (if (kilText.contains("configuration"))
          noModuleWrapper.trim
        else
          (junk.fold(noModuleWrapper) { _.replace(_, "") }).trim)
      }

    val expected =
      if (fullModuleAssertion)
        kastText.trim
      else
        "  " + kastText.trim

    println(actual)
    assertEquals(expected, actual)
  }
}