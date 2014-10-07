package org.kframework.kore

object meta {
  import KORE._
  
  implicit class MetaKLabel(l: KLabel) extends KApplyLike[MetaKLabel] {
    val klabel = KLabel("#klabel")
    val klist = l match {
      case ConcreteKLabel(name) => List(KToken(name, Sort("String")))
      case v: KVariable => List(v) 
    }
    val att = Attributes()
    def copy(items: List[K], att: Attributes) = items match {
      case Seq(KToken(s, Sort("String"), _)) => MetaKLabel(KLabel(s))
    }
  }

  implicit class MetaKList(val l: List[K]) extends KApplyLike[MetaKList] {
    val klabel = KLabel("#klist")
    val klist = l
    val att = Attributes()
    def copy(s: List[K], att: Attributes) = new MetaKList(s)
  }

  implicit class MetaKSeq(val s: KSequence) extends KApplyLike[MetaKSeq] {
    val klabel = KLabel("#kseq")
    val klist = s.items
    val att = s.last.att
    def copy(seq: List[K], att: Attributes) = MetaKSeq(new KSequence(seq, seq.last.att))
  }

  implicit class MetaKVariable(val v: KVariable) extends KApplyLike[MetaKVariable] {
    val klabel = KLabel("#kvariable")
    val klist = List(KToken(v.name, Sort("String")))
    val att = v.att
    def copy(klist: List[K], att: Attributes) = klist match {
      case List(KToken(name, Sort("String"), _)) => MetaKVariable(KVariable(name, att))
    }
  }

  implicit class MetaRewrite(val r: KRewrite) extends KApplyLike[MetaRewrite] {
    val klabel = KLabel("#krewrite")
    val klist = List(r.left, r.right)
    val att = r.att
    def copy(klist: List[K], att: Attributes) = klist match {
      case Seq(left, right) => MetaRewrite(KRewrite(left, right))
    }
  }
}