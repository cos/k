package org.kframework.kore

import KAST._

case class Attributes(m: Map[String, String] = Map())

trait Context

trait Term { def att: Attributes }
trait Terminal extends Term
trait NonTerminal extends Term { def klabel: KLabel; def klist: KList }

object KString { def apply(s: String) = s }

object KAST extends SeqCollectionConstruction {
  type Elem = Term

  case class KApp(klabel: KLabel, klist: KList, att: Attributes = Attributes()) extends Term with NonTerminal
  case class KToken(s: String, sort: String, att: Attributes = Attributes()) extends Term with Terminal
  case class KLabel(name: String)

  final class KList(val items: Seq[Term]) extends SeqCollection[KList] {
    protected val fromSeq = KList.fromSeq
  }
  object KList extends CanBuildKList {
    def apply(KItems: Term*) = new KList(KItems.toIndexedSeq)
  }
}

object KSEQ {
  final class KSeq(val items: Seq[Term], val att: Attributes = Attributes()) extends Term with SeqCollection[KSeq] {
    protected val fromSeq = KSeq.fromSeq
  }
  object KSeq extends CanBuildKSeq {
    def apply(kItems: Term*) = new KSeq(kItems.toIndexedSeq)
  }
}

object K {
  case class KVariable(name: String, att: Attributes = Attributes()) extends Term
  case class Rewrite(left: Term, right: Term, att: Attributes = Attributes()) extends Term
}
