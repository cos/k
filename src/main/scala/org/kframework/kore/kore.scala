package org.kframework.kore

object BASICK {
  object KString {
    def apply(s: String) = s
  }
}

object KAST extends SeqCollectionConstruction {
  import BASICK._

  trait Term

  type Elem = Term

  case class KApp(klabel: KLabel, klist: KList) extends Term

  case class Token(s: String, sort: String) extends Term

  case class KLabel(name: String) extends Term

  final class KList(val items: Seq[Term]) extends Term with SeqCollection[KList] {
    protected val fromSeq = KList.fromSeq _
  }

  object KList extends CanBuildKList {
    def apply(KItems: Term*) = new KList(KItems.toIndexedSeq)
  }
}

object KSEQ {
  import BASICK._
  import KAST._

  trait K extends Term

  final class KSeq(val items: Seq[Term]) extends K with SeqCollection[KSeq] {
    protected val fromSeq = KSeq.fromSeq _
  }

  object KSeq extends CanBuildKSeq {
    def apply(kItems: Term*) = new KSeq(kItems.toIndexedSeq)
  }
}

object K {
  import KAST._
  import KSEQ._

  case class KVariable(name: String)

  case class Rewrite(left: K, right: K)
}
