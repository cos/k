package org.kframework.kast

case class Sort(name: String) {
  override def toString = name
}

trait Sorted {
  val sort: Sort
}

object Sort extends Key[Sort] {
  val key = 'sort

  implicit class SortedTerm(t: Term) extends Sorted {
    val sort = t.attributes(Sort)
  }

  implicit def toSortedTermConstructor[Context](delegate: TermConstructor[Context]) = SortedTermConstructor(delegate)
}

case class SortedTermConstructor[Context](delegate: TermConstructor[Context]) {
  def apply(
    klabel: String,
    klist: Seq[Term],
    sort: Sort,
    attributes: Attributes)(
      implicit context: Context): Term = {
    delegate(klabel, klist, attributes + (Sort -> sort))(context)
  }

  def apply(
    klabel: String,
    klist: Seq[Term],
    sort: Sort,
    attributes: (Symbol, String)*)(
      implicit context: Context): Term = {
    apply(klabel, klist, sort, Attributes(attributes.toMap))(context)
  }
}