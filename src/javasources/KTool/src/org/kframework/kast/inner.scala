package org.kframework.kast

trait KLabel {
  val name: String
  def apply(klist: KItem*) = KApp(this, klist)

  override def toString = name
}

object KItem {
  implicit def toK(t: KItem) = KSeq(Seq(t), t.sort)
}

trait KItem {
  val klabel: KLabel
  val klist: Seq[KItem] = Seq()
  val attributes: Attributes = Attributes(Map())
  val sort: Sort

  override def toString = klabel +
    (if (klist.isEmpty) "" else "(" + klist.sortBy(_.toString).mkString(", ") + ")") +
    ":" + sort +
    (if (attributes.isEmpty) "" else "[" + attributes.toString() + "]")
}

trait K

trait Context {
  val productions: Map[String, Set[Production]]
}

case class Sort(name: String) {
  override def toString = name
}
// MetaKLabel ::= Token{"dummy"}  [regex(`[^`\s]+`)]

case class SimpleKLabel(name: String) extends KLabel {
  //  def apply(params: K*) = KApp(this, params)
}

// MetaKConstant ::= TOKENID "::" SORTID
case class KConstant(klabel: KLabel) extends KItem {
  val sort = Sort("KItem")
}

object KApp {
  def apply(label: KLabel, klist: Seq[KItem]): KApp = {
    KApp(label, klist, Sort("???"))
  }
}

// MetaKItem ::= MetaKLabel "(" MetaKList ")"
case class KApp(klabel: KLabel, override val klist: Seq[KItem], sort: Sort) extends KItem

// MetaKList  ::= NeList{MetaK,","}
//              | ".::MetaKList"
// is just a List[K]

case class Variable(name: String, sort: Sort) extends KItem {
  val klabel = SimpleKLabel(name)
}

object Hole extends KLabel {
  val name = "Hole"
}

// MetaK ::= NeList{MetaKItem,"~>"}
//         | ".::MetaK"

case class KSeq(override val klist: Seq[KItem], sort: Sort) extends KItem with K {
  val klabel = SimpleKLabel("KSeq")
  override def toString = klist.mkString("⤳")
}

// MetaK ::= MetaK "=>" MetaK
case class Rewrite(left: KSeq, right: KSeq) extends KItem with K {
  val klabel = SimpleKLabel("Rewrite")
  val sort = left.sort
  override def toString = left + " => " + right
}

////////
import Definition._

object Boolean {
  def And = SimpleKLabel("'_andBool_")
  def Or = SimpleKLabel("'_orBool_")
}

case class Cell(name: String, content: KItem, override val attributes: Attributes) extends KItem {
  val klabel = SimpleKLabel("<" + name + ">")
  val sort = Sort("Cell")
  override val klist = Seq(content)

  override def toString = "<" + name + ">" + content + "</" + name + ">"
}

case class Bag(override val klist: KItem*) extends KItem {
  val klabel = SimpleKLabel("Bag")
  val sort = Sort("Bag")
}