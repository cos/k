package org.kframework.kore

case class Attributes(m: Map[String, String] = Map())

trait Context

trait Term { def att: Attributes }
trait Terminal extends Term
trait NonTerminal[This <: IndexedSeq[Term]] extends Term with SeqCollection[Term, This] {
  def klabel: KLabel
  def klist: Seq[Term]
  protected val items = klist
}

object KString { def apply(s: String) = s }

case class KApp(klabel: KLabel, klist: Seq[Term], att: Attributes = Attributes()) extends NonTerminal[KApp] {
  protected val fromSeq = { KApp(klabel, _: Seq[Term]) }
}
case class KToken(s: String, sort: String, att: Attributes = Attributes()) extends Terminal
case class KLabel(name: String)

final class KSeq(val items: Seq[Term], val att: Attributes = Attributes()) extends Term with SeqCollection[Term, KSeq] {
  protected val fromSeq = KSeq.fromSeq
}

case class KVariable(name: String, att: Attributes = Attributes()) extends Term
case class Rewrite(left: Term, right: Term, att: Attributes = Attributes()) extends Term

/*  Constructors */

object KSeq extends CanBuildKSeq {
  def apply(kItems: Term*) = new KSeq(kItems.toIndexedSeq)
}
