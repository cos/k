package org.kframework.kast

object Term {
  def unapply(t: Term): Option[(Label, Seq[Term], Attributes)] = {
    import t._
    Some((label, children, attributes))
  }
}

trait Term {
  val label: Label
  val children: Seq[Term]
  val attributes: Attributes

  def copy(children: Seq[Term] = children, attributes: Attributes = attributes): Term = label(children, attributes)
}

trait Term0 extends Term {
  final val children = Seq()
  if (children.size != 0)
    throw new WrongNumberOfChildrenException(children.size, 0)
}

trait Term1 extends Term {
  if (children.size != 1)
    throw new WrongNumberOfChildrenException(children.size, 1)
}

trait Term2 extends Term {
  if (children.size != 2)
    throw new WrongNumberOfChildrenException(children.size, 2)
}

trait TermConstructor[Context] {
  // apply assumentes the klist is clean, i.e., it has been constructed with this TermConstructor
  def apply(klabel: String, klist: Seq[Term], attributes: Attributes)(implicit context: Context): Term

  // could be optimized to not convert things that are already in the format we want
  def convert(termToConvert: Term)(implicit context: Context): Term = {
    import termToConvert._
    apply(label.toString, children.toList map convert, attributes)
  }
}
