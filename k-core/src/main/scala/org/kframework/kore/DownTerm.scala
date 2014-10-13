package org.kframework.kore

class DownTerm(d: outer.Module) {
  def apply(t: K): K = t match {
    case apply: KApply => downterm(apply)
    case k: KCollection[_] with KORE => k map apply
  }

  def downterm(app: KApply): K = {

    ???
  }
}

object DownTerm {
  import outer._
  val hardcodedKoreModule = Module("OUTER-KORE",
    Set(
      SyntaxProduction(Sort("KSentence"), Seq(
        Terminal("syntax"),
        NonTerminal("KSort"),
        Terminal("::="),
        NonTerminal("KProduction"))): Sentence),
    Attributes(
      KApply(KLabel("hook"),
        KList(KToken(Sort("String"), KString("org.kframework.kore.outer.SyntaxProduction"))))))
}