package org.kframework.kast

trait Context

object BasicConstructor extends TermConstructor[Context] {

  object HookClass extends Key[String] { val key = 'hookClass }

  def apply(
    labelString: String,
    children: Seq[Term],
    attributes: Attributes)(
      implicit context: Context): Term = {
    attributes.get(HookClass) match {
      case Some(className) =>
        Class.forName(className).getConstructor(classOf[String]).newInstance(labelString).asInstanceOf[Label](children, attributes)
      case None =>
        val label = labelString match {
          case Rewrite.name => Rewrite
          case KSeq.name => KSeq
          case Hole.name => Hole
          case _ =>
            if (attributes(Flag('variable)))
              Variable(labelString, attributes)
            else if (attributes(Flag('cell)))
              CellLabel(labelString)
            else if (attributes(Flag('bag)))
              BuiltinBagLabel(labelString)
            else if (attributes(Flag('map)))
              BuiltinMapLabel(labelString)
            else if (attributes(Flag('list)))
              BuiltinListLabel(labelString)
            else if (attributes(Flag('set)))
              BuiltinSetLabel(labelString)
            else
              BlandLabel(labelString)
        }

        label(children, attributes)
    }
  }

  def apply(
    labelString: String,
    children: Seq[Term],
    attributes: (Symbol, String)*)(
      implicit context: Context): Term = {
    apply(labelString, children, Attributes(attributes.toMap))
  }
}

object Traversals {
  def collectBF[T](f: PartialFunction[Term, T])(t: Term): Seq[T] = {
    val l = t.children flatMap collectBF(f)
    f.lift(t).map(_ +: l).getOrElse(l)
  }
}

case class BlandLabel(name: String) extends Label {
  def apply(children: Seq[Term], attributes: Attributes): Term = BlandTerm(this, children, attributes)
}

case class BlandTerm(label: Label, children: Seq[Term], attributes: Attributes) extends Term

case class KConstant(name: String, attributes: Attributes) extends LabelTerm with Term0 {
  def apply(children: Seq[Term], attributes: Attributes): Term = {
    KConstant(name, attributes)
  }
}

case class Variable(name: String, val attributes: Attributes) extends LabelTerm with Term0 {
  assert(attributes.get(Sort) != None)
  
  def apply(children: Seq[Term], attributes: Attributes): Term = {
    Variable(name, attributes)
  }
  override def toString = {
    name + attributes.get(Sort).map(":" + _).getOrElse("")
  }
}

object Hole extends Term0 with SingletonLabelTerm {
  val name = "HOLE"
  val attributes = Attributes()
}

object KSeq extends Label {
  val name = "~>"
}

case class KSeq(children: Seq[Term], attributes: Attributes) extends Term {
  val label = KSeq
  override def toString = children.mkString(KSeq.name)
}

object Rewrite extends Label {
  val name = "=>"
  def apply(children: Seq[Term], attributes: Attributes): Term = children match {
    case Seq(left, right) => Rewrite(left, right, attributes)
  }
}

case class Rewrite(left: Term, right: Term, attributes: Attributes) extends {
  val label = Rewrite
  val children = Seq(left, right)
} with Term2 {
  override def toString = left + " => " + right
}

object Boolean {
  val Boolean = Sort("Boolean")
  object And extends Label {
    val name = "'_andBool_"
    def apply(children: Seq[Term], attributes: Attributes): Term = children match {
      case Seq(left, right) => And(left, right, attributes)
    }
  }
  case class And(left: Term, right: Term, attributes: Attributes) extends Term2 {
    val label = And
    val children = Seq(left, right)
  }

  object Or extends Label {
    val name = "'_andBool_"
    def apply(children: Seq[Term], attributes: Attributes): Term = children match {
      case Seq(left, right) => Or(left, right, attributes)
    }
  }
  case class Or(left: Term, right: Term, attributes: Attributes) extends Term2 {
    val label = Or
    val children = Seq(left, right)
  }

  object True extends Term0 with SingletonLabelTerm with NoAttributes {
    val name = "true"
  }
  object False extends Term0 with SingletonLabelTerm with NoAttributes {
    val name = "false"
  }
}

case class CellLabel(name: String) extends Label {
  def apply(children: Seq[Term], attributes: Attributes): Cell = children match {
    case Seq(content) => apply(content, attributes)
  }
  def apply(content: Term, attributes: Attributes): Cell = Cell(this, content, attributes)
}

object Cell {
  def apply(name: String, content: Term, attributes: Attributes): Cell = CellLabel(name)(content, attributes)
}

case class Cell(label: CellLabel, content: Term, attributes: Attributes) extends {
  val name = label.name
  val children = Seq(content)
} with Term1 {
  override def toString = "<" + label.name + ">" + content + "</" + name + ">"
}

case class BuiltinBagLabel(name: String) extends Label {
  def apply(children: Seq[Term], attributes: Attributes): BuiltinBag = BuiltinBag(this, children, attributes)
}

case class BuiltinBag(label: BuiltinBagLabel, children: Seq[Term], attributes: Attributes) extends Term

object BuiltinTuple2Label extends Label {
  val name = "KTuple2"

  def apply(children: Seq[Term], attributes: Attributes): BuiltinTuple2 = children match {
    case Seq(_1, _2) => BuiltinTuple2(_1, _2)
  }
}

case class BuiltinTuple2(_1: Term, _2: Term) extends Term {
  val label = BuiltinTuple2Label
  val children = Seq(_1, _2)
  val attributes = Attributes()
}

case class BuiltinMapLabel(name: String) extends Label {
  def apply(children: Seq[Term], attributes: Attributes): BuiltinMap = {
    val pairs = children map { case BuiltinTuple2(_1, _2) => (_1, _2) } toMap

    BuiltinMap(this, pairs, attributes)
  }
}

case class BuiltinMap(label: BuiltinMapLabel, map: Map[Term, Term], attributes: Attributes) extends Term {
  val children = map.toSeq.map { case (_1, _2) => BuiltinTuple2(_1, _2) }
}

case class BuiltinListLabel(name: String) extends Label {
  def apply(children: Seq[Term], attributes: Attributes): BuiltinList = BuiltinList(this, children, attributes)
}

case class BuiltinList(label: BuiltinListLabel, children: Seq[Term], attributes: Attributes) extends Term

case class BuiltinSetLabel(name: String) extends Label {
  def apply(children: Seq[Term], attributes: Attributes): BuiltinSet = BuiltinSet(this, children toSet, attributes)
}

case class BuiltinSet(label: BuiltinSetLabel, set: Set[Term], attributes: Attributes) extends Term {
  val children = set.toList.sortBy(_.toString)
}

//case class SetLabel(name: String) extends Label {
//  def apply(children: Seq[Term], attributes: Attributes): Set = Set(this, children.toSet, attributes)
//}
//
//case class Set(label: SetLabel, set: collection.Set[Term], attributes: Attributes) extends Term {
//  lazy val children = set.toSeq.sortBy(_.toString)
//} 
