package org.kframework.kast

object SymbolicConstraint extends Key[Term] {
  val key = 'constraint
}

trait HasSymbolicConstraint extends Any {
  def constraint: Term
}

object HasSymbolicConstraint {
  implicit class TermHasSymbolicConstraint(val term: Term)
    extends AnyVal with HasSymbolicConstraint {
    def constraint = term.attributes(SymbolicConstraint)
  }
}