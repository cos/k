package org.kframework.kore

import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import scala.collection.IndexedSeqLike
import scala.collection.mutable.ListBuffer

object KORE {
  type KList = List[K]
}

trait GenericCollection[Elem, This <: IndexedSeq[Elem]] extends IndexedSeq[Elem] with IndexedSeqLike[Elem, This] {
  protected val items: List[Elem]
  override protected [this] def newBuilder: Builder[Elem, This] =
    GenericCollection.newBuilder(copy)

  def copy(s: List[Elem]): This

  def apply(idx: Int) = items(idx)
  def length = items.length
}

object GenericCollection {
  def newBuilder[Elem, C <: IndexedSeq[Elem]](fromList: List[Elem] => C): Builder[Elem, C] = new ListBuffer mapResult fromList
}

trait CanBuild[T <: Collection[T]] {
  def apply(seq: List[K], att: Attributes): T

  def apply(kItems: K*): T = apply(kItems.toList, Attributes())

  val copy = apply(_: List[K], _: Attributes)

  implicit def canBuildFrom: CanBuildFrom[T, K, T] =
    new CanBuildFrom[T, K, T] {
      def apply(): Builder[K, T] = GenericCollection.newBuilder[K, T](copy(_: List[K], Attributes()))
      def apply(from: T): Builder[K, T] = GenericCollection.newBuilder(copy(_: List[K], from.att))
    }
}