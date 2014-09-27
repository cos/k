package org.kframework.kore

import scala.collection.mutable.Builder
import scala.collection.mutable.ArrayBuffer
import scala.collection.generic.CanBuildFrom
import scala.collection.IndexedSeqLike

trait SeqCollectionConstruction {
  type Elem

  trait SeqCollection[C <: IndexedSeq[Elem]] extends IndexedSeq[Elem] with IndexedSeqLike[Elem, C] {
    val items: Seq[Elem]
    override protected[this] def newBuilder: Builder[Elem, C] =
      SeqCollection.newBuilder(fromSeq)

    protected val fromSeq: Seq[Elem] => C

    def apply(idx: Int) = items(idx)
    def length = items.length
  }

  object SeqCollection {
    def newBuilder[C](fromSeq: Seq[Elem] => C): Builder[Elem, C] = new ArrayBuffer mapResult fromSeq
  }
}

trait CanBuildKList {
  import KAST._

  val fromSeq = new KList(_)

  implicit def canBuildFrom: CanBuildFrom[KList, Term, KList] =
    new CanBuildFrom[KList, Term, KList] {
      def apply(): Builder[Term, KList] = SeqCollection.newBuilder(fromSeq)
      def apply(from: KList): Builder[Term, KList] = SeqCollection.newBuilder(fromSeq)
    }
}

trait CanBuildKSeq {
  import KAST._
  import KSEQ._

  val fromSeq = new KSeq(_: Seq[Term])

  implicit def canBuildFrom: CanBuildFrom[KSeq, Term, KSeq] =
    new CanBuildFrom[KSeq, Term, KSeq] {
      def apply(): Builder[Term, KSeq] = SeqCollection.newBuilder(fromSeq)
      def apply(from: KSeq): Builder[Term, KSeq] = SeqCollection.newBuilder(fromSeq)
    }
}