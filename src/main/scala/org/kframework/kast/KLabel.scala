package org.kframework.kast

object KLabel {
  def unapply(klabel: KLabel): Option[String] = Some(klabel.name)
}

trait KLabel {
  val name: String
  def apply(klist: Seq[Term], attributes: Attributes): Term

  override def toString = name
}

class WrongNumberOfChildrenException(actual: Int, expected: Int) extends RuntimeException(s"Wrong number of children. Actual: $actual Expected:$expected")

trait KLabelTerm extends KLabel with Term {
  final val klabel = this
}

trait SingletonKLabelTerm extends KLabelTerm {
  def apply(klist: Seq[Term], attributes: Attributes): Term = this
}
