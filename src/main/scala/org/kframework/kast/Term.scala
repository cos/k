package org.kframework.kast

object Term {
  def unapply(t: Term): Option[(KLabel, Seq[Term], Attributes)] = {
    import t._
    Some((klabel, klist, attributes))
  }
}

trait Term {
  val klabel: KLabel
  val klist: scala.Seq[Term]
  val attributes: Attributes

  def copy(klist: Seq[Term] = klist, attributes: Attributes = attributes): Term = klabel(klist, attributes)
}

trait Term0 extends Term {
  final val klist = Seq()
  if (klist.size != 0)
    throw new WrongNumberOfChildrenException(klist.size, 0)
}

trait Term1 extends Term {
  if (klist.size != 1)
    throw new WrongNumberOfChildrenException(klist.size, 1)
}

trait Term2 extends Term {
  if (klist.size != 2)
    throw new WrongNumberOfChildrenException(klist.size, 2)
}

trait TermConstructor[Context] {
  // apply assumentes the klist is clean, i.e., it has been constructed with this TermConstructor
  def apply(klabel: String, klist: Seq[Term], attributes: Attributes)(implicit context: Context): Term

  // could be optimized to not convert things that are already in the format we want
  def convert(termToConvert: Term)(implicit context: Context): Term = {
    import termToConvert._
    apply(klabel.toString, klist.toList map convert, attributes)
  }
}
