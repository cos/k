package org.kframework.kast

trait Context

object BasicConstructor extends TermConstructor[Context] {
  def apply(
    klabelString: String,
    klist: Seq[Term],
    attributes: Attributes)(
      implicit context: Context): Term = {
    attributes.get('hookClass) match {
      case Some(className) =>
        Class.forName(className).getConstructor(classOf[String]).newInstance(klabelString).asInstanceOf[KLabel](klist, attributes)
      case None =>
        val klabel = klabelString match {
          case "=>" => Rewrite
          case "⤳" => KSeq
          case "HOLE" => Hole
          case _ =>
            attributes.get('variable).map(on => Variable(klabelString, attributes))
              .orElse(attributes.get('cell).map(on => CellLabel(klabelString)))
              .orElse(attributes.get('bag).map(on => BagLabel(klabelString)))
              .getOrElse(BlandKLabel(klabelString))
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
    name + attributes.get('sort).map(":" + _).getOrElse("")
  }
}

object Hole extends Term0 with KLabelTerm with SingletonTerm {
  val name = "HOLE"
  val attributes = Attributes()
}

object KSeq extends KLabel {
  val name = "⤳"
}

case class KSeq(klist: Seq[Term], attributes: Attributes) extends Term {
  val klabel = KSeq
  override def toString = klist.mkString("⤳")
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

////////
import Definition._

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

  object True extends Term0 with KLabelTerm with SingletonTerm with NoAttributes {
    val name = "true"
  }
  object False extends Term0 with KLabelTerm with SingletonTerm with NoAttributes {
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

case class BagLabel(name: String) extends KLabel {
  def apply(klist: Seq[Term], attributes: Attributes): Bag = Bag(this, klist, attributes)
}

case class Bag(klabel: BagLabel, klist: Seq[Term], attributes: Attributes) extends Term
