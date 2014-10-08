package org.kframework.kore

trait HasToConstructor {
  def toConstructor: String
}

object HasToConstructor {
  implicit def KHasToConstructor(k: K): HasToConstructor = k match {
    case kapply: KApply => new KApplyHasToConstructor(kapply)
    case ktoken: KToken => new KTokenHasToConstructor(ktoken)
  }

  implicit def KLabelHasToConstructor(l: KLabel): HasToConstructor = l match {
    case kapply: ConcreteKLabel => new ConcreteKLabelHasToConstructor(kapply)
  }

  implicit class KTokenHasToConstructor(t: KToken) extends HasToConstructor {
    def toConstructor = "KToken(" +
      t.sort.toConstructor + ", " +
      t.s.toConstructor + ")"
  }

  implicit class KStringHasToConstructor(s: KString) extends HasToConstructor {
    def toConstructor = "KString(" + s.s.toConstructor + ")"
  }
  
  implicit class StringHasToConstructor(s: String) extends HasToConstructor {
    def toConstructor = "\"" + s + "\""
  }

  implicit class SortHasToConstructor(s: Sort) extends HasToConstructor {
    def toConstructor = "Sort(" + s.name.toConstructor + ")"
  }

  implicit class ConcreteKLabelHasToConstructor(klabel: ConcreteKLabel) extends HasToConstructor {
    def toConstructor = "KLabel(" + klabel.name.toConstructor + ")"
  }

  implicit class KApplyHasToConstructor(kapply: KApplyLike[_]) extends HasToConstructor {
    def toConstructor = "KApply(" +
      kapply.klabel.toConstructor + "," + "List[K](" +
      (kapply.klist map { _.toConstructor }).mkString(", ") + "))"
  }
}
