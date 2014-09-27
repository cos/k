package org.kframework.kore

import scala.collection.mutable.Builder
import scala.collection.mutable.ArrayBuffer
import scala.collection.generic.CanBuildFrom
import scala.collection.IndexedSeqLike

trait SeqCollection[Elem, This <: IndexedSeq[Elem]] extends IndexedSeq[Elem] with IndexedSeqLike[Elem, This] {
  protected val items: Seq[Elem]
  override protected[this] def newBuilder: Builder[Elem, This] =
    SeqCollection.newBuilder(fromSeq)

  protected val fromSeq: Seq[Elem] => This

  def apply(idx: Int) = items(idx)
  def length = items.length
}

object SeqCollection {
  def newBuilder[Elem, C <: IndexedSeq[Elem]](fromSeq: Seq[Elem] => C): Builder[Elem, C] = new ArrayBuffer mapResult fromSeq
}

trait CanBuildKSeq {
  val fromSeq: Seq[Term] => KSeq = new KSeq(_: Seq[Term])

  implicit def canBuildFrom: CanBuildFrom[KSeq, Term, KSeq] =
    new CanBuildFrom[KSeq, Term, KSeq] {
      def apply(): Builder[Term, KSeq] = SeqCollection.newBuilder[Term, KSeq](fromSeq)
      def apply(from: KSeq): Builder[Term, KSeq] = SeqCollection.newBuilder(fromSeq)
    }
}