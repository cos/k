package org.kframework.kore

object meta {
  implicit class MetaKLabel(l: KLabel) extends KAppLike[MetaKLabel] {
    val klabel = KLabel("#klabel")
    val klist = l match {
      case ConcreteKLabel(name) => List(KToken(name, "String"))
      case v: KVariable => List(v) 
    }
    val att = Attributes()
    def copy(items: List[K], att: Attributes) = items match {
      case Seq(KToken(s, "String", _)) => MetaKLabel(KLabel(s))
    }
  }

  implicit class MetaKList(val l: List[K]) extends KAppLike[MetaKList] {
    val klabel = KLabel("#klist")
    val klist = l
    val att = Attributes()
    def copy(s: List[K], att: Attributes) = new MetaKList(s)
  }

  implicit class MetaKSeq(val s: KSequence) extends KAppLike[MetaKSeq] {
    val klabel = KLabel("#kseq")
    val klist = s.items
    val att = s.last.att
    def copy(seq: List[K], att: Attributes) = MetaKSeq(new KSequence(seq, seq.last.att))
  }

  implicit class MetaKVariable(val v: KVariable) extends KAppLike[MetaKVariable] {
    val klabel = KLabel("#kvariable")
    val klist = List(KToken(v.name, "String"))
    val att = v.att
    def copy(klist: List[K], att: Attributes) = klist match {
      case List(KToken(name, "String", _)) => MetaKVariable(KVariable(name, att))
    }
  }

  implicit class MetaRewrite(val r: KRewrite) extends KAppLike[MetaRewrite] {
    val klabel = KLabel("#krewrite")
    val klist = List(r.left, r.right)
    val att = r.att
    def copy(klist: List[K], att: Attributes) = klist match {
      case Seq(left, right) => MetaRewrite(KRewrite(left, right))
    }
  }
}