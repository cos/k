package org.kframework.kore

case class Attributes(m: Map[String, String] = Map())

trait Context

import KORE._

trait HasAttributes {
  def att: Attributes
}

trait K extends HasAttributes

trait Collection[This <: IndexedSeq[K]] extends GenericCollection[K, This] with HasAttributes {
  def copy(klist: KList, att: Attributes): This
  def copy(klist: KList): This = copy(klist, att)
}

object KString { def apply(s: String) = s }

trait KItem extends K

trait KAppLike[This <: IndexedSeq[K]] extends KItem with Collection[This] with HasAttributes {
  def klabel: KLabel
  def klist: KList
  protected val items = klist
}

case class KApply(klabel: KLabel, klist: KList, att: Attributes = Attributes()) extends KItem with KAppLike[KApply] with Collection[KApply] {
  def copy(klist: KList, att: Attributes) = KApply(klabel, klist, att)
}

case class KToken(s: String, sort: String, att: Attributes = Attributes()) extends KItem

trait KLabel

object KLabel {
  def apply(name: String) = ConcreteKLabel(name)
}

case class ConcreteKLabel(name: String) extends KLabel

case class EmptyK(att: Attributes = Attributes()) extends K

final class KSequence(val items: KList, val att: Attributes = Attributes()) extends Collection[KSequence] {
  def copy(klist: KList, att: Attributes): KSequence = KSequence(klist, att)
}

case class KVariable(name: String, att: Attributes = Attributes()) extends KItem

case class KRewrite(left: K, right: K, att: Attributes = Attributes()) extends K with Collection[KRewrite] {
  def copy(klist: KList, att: Attributes): KRewrite = KRewrite(klist, att)
  val items = List(left, right)
}

/*  Constructors */

object KSequence extends CanBuild[KSequence] {
  def apply(klist: KList, att: Attributes) = new KSequence(klist, att)
}

object KRewrite extends CanBuild[KRewrite] {
  def apply(klist: KList, att: Attributes) = klist match {
    case Seq(left, right) => new KRewrite(left, right, att)
  }
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
object KList {
  def apply(l: KList) = l
  implicit def inject(k: K): KList = List(k)
}
