package org.kframework.kast.outer

trait PrettyPrintable {
  def pp: String
}

object PrettyPrintableToKAST {
  implicit class PrettyModule(m: Module) {
    def pp = "module " + m.name + "\n" + m.sentences.mkString("\n\n") + "\n\nendmodule"
  }
}