package org.kframework.kore

import KAST._

trait Meta extends NonTerminal {
  def klabel: KLabel
  def klist: KList

  def asKApp: KApp = KApp(klabel, klist)
}

object MetaKAST {
  implicit class MetaKLabel(val l: KLabel) extends Meta {
    val klabel = KLabel("#klabel")
    val klist = KList(KToken(l.name, "String"))
    val att = ???
  }

  implicit class MetaKList(val l: KList) extends Meta {
    val klabel = KLabel("#klist")
    val klist = l
    val att = ???
  }
}

object MetaKSEQ {
  import KSEQ._

  implicit class MetaKSeq(val s: KSeq) extends Meta {
    val klabel = KLabel("#kseq")
    val klist = new KList(s.items)
    val att = s.tail.att
  }
}

object MetaK {
  import K._

  implicit class MetaKVariable(val v: KVariable) extends Meta {
    val klabel = KLabel("#kvariable")
    val klist = KList(KToken(v.name, "KVariable"))
    val att = v.att
  }

  implicit class MetaRewrite(val r: Rewrite) extends Meta {
    val klabel = KLabel("#krewrite")
    val klist = KList(r.left, r.right)
    val att = r.att
  }
}
