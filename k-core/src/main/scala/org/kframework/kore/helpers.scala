package org.kframework.kore

import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import scala.collection.LinearSeqLike
import scala.collection.mutable.ListBuffer
import KORE._
import scala.collection.LinearSeq

// not sure this is the best approach -- might want a LinearSeqOptimized instead
trait GenericCollection[Elem, +This <: GenericCollection[Elem, This]] extends LinearSeq[Elem] with LinearSeqLike[Elem, This] {
  self: This =>
  protected val items: Seq[Elem]
  override protected[this] def newBuilder: Builder[Elem, This] =
    GenericCollection.newBuilder(copy)

  def copy(s: Seq[Elem]): This

  def apply(idx: Int) = items(idx)
  def length = items.length
}

object GenericCollection {
  def newBuilder[Elem, C <: LinearSeq[Elem]](fromList: List[Elem] => C): Builder[Elem, C] = new ListBuffer mapResult fromList
}

trait Collection[This <: Collection[This]] extends GenericCollection[K, This] with HasAttributes {
  self: This =>
  def copy(klist: KList, att: Attributes): This
  def copy(klist: KList): This = copy(klist, att)
  def copy(klist: Seq[K]): This = copy(new KList(klist), att)
}

trait CanBuild[T <: Collection[T]] {
  def apply(seq: KList, att: Attributes): T

  def apply(kItems: K*): T = apply(new KList(kItems), Attributes())

  def copy(seq: Seq[K], att: Attributes) = apply(new KList(seq), att)

  implicit def canBuildFrom: CanBuildFrom[T, K, T] =
    new CanBuildFrom[T, K, T] {
      def apply(): Builder[K, T] = GenericCollection.newBuilder[K, T](copy(_: List[K], Attributes()))
      def apply(from: T): Builder[K, T] = GenericCollection.newBuilder(copy(_: List[K], from.att))
    }
}

trait Context

object KORE {
  implicit def StringToKString(s: String) = KString(s)
}

// will probably get rid of this if going for the simplified meta-level
trait KApplyLike[This <: KApplyLike[This]] extends KItem with Collection[This] with HasAttributes {
  self: This =>
  def klabel: KLabel
  def klist: KList
  protected val items = klist
}