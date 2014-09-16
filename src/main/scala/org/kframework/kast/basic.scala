package org.kframework.kast

trait Context

object BasicConstructor extends TermConstructor[Context] {

  object HookClass extends Key[String] { val key = 'hookClass }

  def apply(
    klabelString: String,
    klist: Seq[Term],
    attributes: Attributes)(
      implicit context: Context): Term = {
    attributes.get(HookClass) match {
      case Some(className) =>
        Class.forName(className).getConstructor(classOf[String]).newInstance(klabelString).asInstanceOf[KLabel](klist, attributes)
      case None =>
        val klabel = klabelString match {
          case Rewrite.name => Rewrite
          case KSeq.name => KSeq
          case Hole.name => Hole
          case _ =>
            if (attributes(Flag('variable)))
              Variable(klabelString, attributes)
            else if (attributes(Flag('cell)))
              CellLabel(klabelString)
            else if (attributes(Flag('bag)))
              BuiltinBagLabel(klabelString)
            else if (attributes(Flag('map)))
              BuiltinMapLabel(klabelString)
            else if (attributes(Flag('list)))
              BuiltinListLabel(klabelString)
            else if (attributes(Flag('set)))
              BuiltinSetLabel(klabelString)
            else
              BlandKLabel(klabelString)
        }

        klabel(klist, attributes)
    }
  }

  def apply(
    klabelString: String,
    klist: Seq[Term],
    attributes: (Symbol, String)*)(
      implicit context: Context): Term = {
    apply(klabelString, klist, Attributes(attributes.toMap))
  }
}

object Traversals {
  def collectBF[T](f: PartialFunction[Term, T])(t: Term): Seq[T] = {
    val l = t.klist flatMap collectBF(f)
    f.lift(t).map(_ +: l).getOrElse(l)
  }
}

case class BlandKLabel(name: String) extends KLabel {
  def apply(klist: Seq[Term], attributes: Attributes): Term = BlandTerm(this, klist, attributes)
}

case class BlandTerm(klabel: KLabel, klist: Seq[Term], attributes: Attributes) extends Term

case class KConstant(name: String, attributes: Attributes) extends KLabelTerm with Term0 {
  def apply(klist: Seq[Term], attributes: Attributes): Term = {
    KConstant(name, attributes)
  }
}

case class Variable(name: String, val attributes: Attributes) extends KLabelTerm with Term0 {
  def apply(klist: Seq[Term], attributes: Attributes): Term = {
    Variable(name, attributes)
  }
  override def toString = {
    name + attributes.get(Sort).map(":" + _).getOrElse("")
  }
}

object Hole extends Term0 with SingletonKLabelTerm {
  val name = "HOLE"
  val attributes = Attributes()
}

object KSeq extends KLabel {
  val name = "~>"
}

case class KSeq(klist: Seq[Term], attributes: Attributes) extends Term {
  val klabel = KSeq
  override def toString = klist.mkString(KSeq.name)
}

object Rewrite extends KLabel {
  val name = "=>"
  def apply(klist: Seq[Term], attributes: Attributes): Term = klist match {
    case Seq(left, right) => Rewrite(left, right, attributes)
  }
}

case class Rewrite(left: Term, right: Term, attributes: Attributes) extends {
  val klabel = Rewrite
  val klist = Seq(left, right)
} with Term2 {
  override def toString = left + " => " + right
}

object Boolean {
  val Boolean = Sort("Boolean")
  object And extends KLabel {
    val name = "'_andBool_"
    def apply(klist: Seq[Term], attributes: Attributes): Term = klist match {
      case Seq(left, right) => And(left, right, attributes)
    }
  }
  case class And(left: Term, right: Term, attributes: Attributes) extends Term2 {
    val klabel = And
    val klist = Seq(left, right)
  }

  object Or extends KLabel {
    val name = "'_andBool_"
    def apply(klist: Seq[Term], attributes: Attributes): Term = klist match {
      case Seq(left, right) => Or(left, right, attributes)
    }
  }
  case class Or(left: Term, right: Term, attributes: Attributes) extends Term2 {
    val klabel = Or
    val klist = Seq(left, right)
  }

  object True extends Term0 with SingletonKLabelTerm with NoAttributes {
    val name = "true"
  }
  object False extends Term0 with SingletonKLabelTerm with NoAttributes {
    val name = "false"
  }
}

case class CellLabel(name: String) extends KLabel {
  def apply(klist: Seq[Term], attributes: Attributes): Cell = klist match {
    case Seq(content) => apply(content, attributes)
  }
  def apply(content: Term, attributes: Attributes): Cell = Cell(this, content, attributes)
}

object Cell {
  def apply(name: String, content: Term, attributes: Attributes): Cell = CellLabel(name)(content, attributes)
}

case class Cell(klabel: CellLabel, content: Term, attributes: Attributes) extends {
  val name = klabel.name
  val klist = Seq(content)
} with Term1 {
  override def toString = "<" + klabel.name + ">" + content + "</" + name + ">"
}

case class BuiltinBagLabel(name: String) extends KLabel {
  def apply(klist: Seq[Term], attributes: Attributes): BuiltinBag = BuiltinBag(this, klist, attributes)
}

case class BuiltinBag(klabel: BuiltinBagLabel, klist: Seq[Term], attributes: Attributes) extends Term

object BuiltinTuple2Label extends KLabel {
  val name = "KTuple2"

  def apply(klist: Seq[Term], attributes: Attributes): BuiltinTuple2 = klist match {
    case Seq(_1, _2) => BuiltinTuple2(_1, _2)
  }
}

case class BuiltinTuple2(_1: Term, _2: Term) extends Term {
  val klabel = BuiltinTuple2Label
  val klist = Seq(_1, _2)
  val attributes = Attributes()
}

case class BuiltinMapLabel(name: String) extends KLabel {
  def apply(klist: Seq[Term], attributes: Attributes): BuiltinMap = {
    val pairs = klist map { case BuiltinTuple2(_1, _2) => (_1, _2) } toMap

    BuiltinMap(this, pairs, attributes)
  }
}

case class BuiltinMap(klabel: BuiltinMapLabel, map: Map[Term, Term], attributes: Attributes) extends Term {
  val klist = map.toSeq.map { case (_1, _2) => BuiltinTuple2(_1, _2) }
}

case class BuiltinListLabel(name: String) extends KLabel {
  def apply(klist: Seq[Term], attributes: Attributes): BuiltinList = BuiltinList(this, klist, attributes)
}

case class BuiltinList(klabel: BuiltinListLabel, klist: Seq[Term], attributes: Attributes) extends Term

case class BuiltinSetLabel(name: String) extends KLabel {
  def apply(klist: Seq[Term], attributes: Attributes): BuiltinSet = BuiltinSet(this, klist toSet, attributes)
}

case class BuiltinSet(klabel: BuiltinSetLabel, set: Set[Term], attributes: Attributes) extends Term {
  val klist = set.toList.sortBy(_.toString)
}

//case class SetLabel(name: String) extends KLabel {
//  def apply(klist: Seq[Term], attributes: Attributes): Set = Set(this, klist.toSet, attributes)
//}
//
//case class Set(klabel: SetLabel, set: collection.Set[Term], attributes: Attributes) extends Term {
//  lazy val klist = set.toSeq.sortBy(_.toString)
//} 
