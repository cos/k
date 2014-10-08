package org.kframework.kore

import org.kframework.kil
import collection.JavaConversions._

object TermConsToKAST {
  import KORE._

  def visit(term: kil.Term): K = term match {
    case t: kil.TermCons =>
      val attributesAsK =
        t.getAttributes() map {
          case (_, a) => KApply(KLabel(a.getKey().toString()),
            List(KToken(Sort("String"), KString(a.getValue().toString()))))
        }

      val loc = t.getLocation()
      val startLine = loc.lineStart

      val attWithLocationInfo = attributesAsK ++ List(
        KLabel("location")(
          KLabel("startLine")(KToken(Sort("Int"), KString(loc.lineStart.toString))),
          KLabel("startColumn")(KToken(Sort("Int"), KString(loc.columnStart.toString))),
          KLabel("endLine")(KToken(Sort("Int"), KString(loc.lineEnd.toString))),
          KLabel("endColumn")(KToken(Sort("Int"), KString(loc.columnEnd.toString)))))

      KApply(
        KLabel(t.getProduction().getKLabel()),
        t.getContents() map { visit(_) } toList,
        Attributes(attWithLocationInfo toList))
    case t: kil.Constant =>
      KToken(Sort(t.getSort().getName()), KString(t.getValue()))
  }
}
