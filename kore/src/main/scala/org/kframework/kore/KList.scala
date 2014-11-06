// Copyright (c) 2014 K Team. All Rights Reserved.

package org.kframework.kore

import collection.{ AbstractSeq, LinearSeq, LinearSeqOptimized, Seq, generic, mutable }

abstract class KList extends KListLike[KList] {
  def copy(l: LinearSeq[K]) = KList(l: _*)
}

final case object EmptyKList extends KList with Serializable {
  override def isEmpty = true
  override def head: Nothing = throw new NoSuchElementException("head of empty klist")
  override def tail: KList = throw new UnsupportedOperationException("tail of empty klist")
  override def equals(that: Any) = that match {
    case that: scala.collection.GenSeq[_] => that.isEmpty
    case _ => false
  }
}

final case class ConsKList(override val head: K, override val tail: KList) extends KList {
  override def isEmpty = false
}

object KList extends CanBuildKListLike[KList] {
  def apply(l: K*): KList = l.foldLeft(EmptyKList: KList) { (l: KList, h: K) => new ConsKList(h, l) }

  implicit def inject(k: K): KList = KList(k)
  implicit def seqOfKtoKList(s: Seq[K]) = KList(s: _*)
  def fromJava(l: Array[K]) = apply(l: _*)
  val fromList = seqOfKtoKList _

  def unapplySeq(l: KList): Option[Seq[K]] = Some(l)
}

/**
 * Describes objects which contain K's and can be iterated like a KList
 */
trait KListLike[+This <: KListLike[This]] extends LinearSeq[K] with LinearSeqOptimized[K, This] {
  self: This =>
  override def newBuilder: mutable.Builder[K, This] =
    new mutable.ListBuffer mapResult copy

  def copy(l: LinearSeq[K]): This
}

/**
 * A KList-backed implementation of KList
 *
 * Implementing classes only need to provide an implementation of:
 * def copy(LinearSeq[K]): This
 *
 * @see org.kframework.kore.Attributes for an example
 *
 * KList is not based on this class because it cannot be backed by itself
 *
 */
trait KListBacked[+This <: KListLike[This]] extends KListLike[This] {
  self: This =>
  val klist: KList

  override def head = klist.head
  override def tail = copy(klist.tail)
  override def isEmpty = klist.isEmpty
}

/**
 * Should be extended by companion objects of classes extending KListLike
 */
trait CanBuildKListLike[This <: KListLike[This]] {
  def apply(l: K*): This

  private val fromList = apply _

  implicit def canBuildFrom: generic.CanBuildFrom[This, K, This] =
    new generic.CanBuildFrom[This, K, This] {
      def apply(): mutable.Builder[K, This] = new mutable.ListBuffer mapResult fromList
      def apply(from: This): mutable.Builder[K, This] = from.newBuilder
    }
}