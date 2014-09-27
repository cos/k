package org.kframework.kore

import KAST._

trait Meta extends Any {
  def klabel: KLabel
  def klist: KList

  def asKApp: KApp = KApp(klabel, klist)
}

object MetaKAST {
  implicit class MetaToken(val token: Token) extends AnyVal with Meta {
    def klabel = KLabel("#ktoken")
    def klist = KList(Token(token.s, "String"), Token(token.sort, "Sort"))
  }

  implicit class MetaKApp(val kapp: KApp) extends AnyVal with Meta {
    def klabel = kapp.klabel
    def klist = kapp.klist
  }

  implicit class MetaKLabel(val l: KLabel) extends AnyVal with Meta {
    def klabel = KLabel("#klabel")
    def klist = KList(Token(l.name, "String"))
  }

  implicit class MetaKList(val l: KList) extends AnyVal with Meta {
    def klabel = KLabel("#klist")
    def klist = l
  }
}

object MetaKSEQ {
  import KSEQ._

  implicit class MetaKSeq(val s: KSeq) extends AnyVal with Meta {
    def klabel = KLabel("#kseq")
    def klist = new KList(s.items)
  }
}

object MetaK {
  import K._

  implicit class MetaKVariable(val v: KVariable) extends AnyVal with Meta {
    def klabel = KLabel("#kvariable")
    def klist = KList(Token(v.name, "KVariable"))
  }

  implicit class MetaRewrite(val r: Rewrite) extends AnyVal with Meta {
    def klabel = KLabel("#krewrite")
    def klist = KList(r.left, r.right)
  }
}
