package org.kframework.kast

object Label {
  def unapply(klabel: Label): Option[String] = Some(klabel.name)
}

trait Label {
  val name: String
  def apply(klist: Seq[Term], attributes: Attributes): Term

  override def toString = name
}

class WrongNumberOfChildrenException(actual: Int, expected: Int) extends RuntimeException(s"Wrong number of children. Actual: $actual Expected:$expected")

trait LabelTerm extends Label with Term {
  final val label = this
}

trait SingletonLabelTerm extends LabelTerm {
  def apply(klist: Seq[Term], attributes: Attributes): Term = this
}