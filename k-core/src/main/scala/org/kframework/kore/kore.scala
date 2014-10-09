package org.kframework.kore

import KORE._

/* Interfaces */

trait HasAttributes {
  def att: Attributes
}

trait K extends HasAttributes

trait KItem extends K

trait KLabel // needs to be a KLabel to be able to have KVariables in its place

/* Data Structures */

final class Attributes(val items: Seq[K]) extends GenericCollection[K, Attributes] {
  // we will eventually decide on something much more specific for attributes
  def copy(klist: Seq[K]) = new Attributes(klist)
}

final class KList(val items: Seq[K]) extends GenericCollection[K, KList] {
  def copy(klist: Seq[K]) = new KList(items)
}

case class KString(s: String) // just a wrapper to mark it

case class KApply(klabel: KLabel, klist: KList, att: Attributes = Attributes()) extends KApplyLike[KApply] {
  def copy(klist: KList, att: Attributes) = KApply(klabel, klist, att)
}

case class KToken(sort: Sort, s: KString, att: Attributes = Attributes()) extends KItem

case class ConcreteKLabel(name: String) extends KLabel {
  def apply(ks: K*) = KApply(this, KList(ks: _*))
}

case class EmptyK(att: Attributes = Attributes()) extends K // we might want an empty KSequence instead

final class KSequence(val items: KList, val att: Attributes = Attributes()) extends K with Collection[KSequence] {
  def copy(klist: KList, att: Attributes): KSequence = KSequence(klist, att)
}

case class KVariable(name: String, att: Attributes = Attributes()) extends KItem

case class KRewrite(left: K, right: K, att: Attributes = Attributes()) extends K with Collection[KRewrite] {
  def copy(klist: KList, att: Attributes): KRewrite = KRewrite(klist, att)
  val items = List(left, right)
}

/*  Constructors */

object Attributes {
  def apply(klist: KList): Attributes = new Attributes(klist)
  def apply(): Attributes = new Attributes(KList())
}

object KList {
  def apply(l: K*) = new KList(l.toList)
  implicit def inject(k: K): KList = KList(k)
}

object KSequence extends CanBuild[KSequence] {
  def apply(klist: KList, att: Attributes) = new KSequence(klist, att)
}

object KRewrite extends CanBuild[KRewrite] {
  def apply(klist: KList, att: Attributes) = klist match {
    case Seq(left, right) => new KRewrite(left, right, att)
  }
}

object KLabel {
  def apply(name: String) = ConcreteKLabel(name)
}

/* Constructors for matching KORE */

object KLabelWithQuotes {
  def apply(s: String) = {
    KLabel(s.stripPrefix("`").stripSuffix("`"))
  }
}

object EmptyKList {
  def apply() = List[K]()
}

case class Sort(name: String)

