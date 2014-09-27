package org.kframework.kore

object Meta {
  implicit class MetaKLabel(val l: KLabel) extends NonTerminal[MetaKLabel] {
    val klabel = KLabel("#klabel")
    val klist = Seq(KToken(l.name, "String"))
    val att = ???
    val fromSeq: Seq[Term] => MetaKLabel = {
      case Seq(KToken(s, "String", _)) => MetaKLabel(KLabel(s))
    }
  }

  implicit class MetaKList(val l: Seq[Term]) extends NonTerminal[MetaKList] {
    val klabel = KLabel("#klist")
    val klist = l
    val att = ???
    val fromSeq = MetaKList(_)
  }

  implicit class MetaKSeq(val s: KSeq) extends NonTerminal[MetaKSeq] {
    val klabel = KLabel("#kseq")
    val klist = s.items
    val att = s.last.att
    val fromSeq = { seq: Seq[Term] => MetaKSeq(new KSeq(seq, seq.last.att)) }
  }

  implicit class MetaKVariable(val v: KVariable) extends NonTerminal[MetaKVariable] {
    val klabel = KLabel("#kvariable")
    val klist = Seq(KToken(v.name, "String"))
    val att = v.att
    val fromSeq: Seq[Term] => MetaKVariable = {
      case Seq(KToken(name, "String", _)) => MetaKVariable(KVariable(name, att))
    }
  }

  implicit class MetaRewrite(val r: Rewrite) extends NonTerminal[MetaRewrite] {
    val klabel = KLabel("#krewrite")
    val klist = Seq(r.left, r.right)
    val att = r.att
    val fromSeq: Seq[Term] => MetaRewrite = {
      case Seq(left, right) => MetaRewrite(Rewrite(left, right))
    }
  }
}