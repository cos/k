package org.kframework.kast

case class Sort(name: String) {
  override def toString = name
}

trait Sorted {
  val sort: Sort
}

object Sort {
  implicit class SortedTerm(t: Term) extends Sorted {
    val sort = Sort(t.attributes('sort))
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
    delegate(klabel, klist, attributes + ('sort -> sort.toString()))(context)
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