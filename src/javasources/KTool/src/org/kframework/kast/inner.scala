package org.kframework.kast

trait KLabel {
  val name: String
  def apply(klist: K*) = KApp(this, klist)
  
  override def toString = name
}

object KItem {
  implicit def toK(kitem: KItem) = Heated(Seq(kitem))
}

trait KItem {
  val label: KLabel
  val items: Seq[K] = Seq()
  val meta: Map[Symbol, Any] = Map()
  val sort: Sort

  override def toString = label +
    (if (items.isEmpty) "" else "(" + items.mkString(", ") + ")") + ":" + sort +
    (if (meta.isEmpty) "" else "{" + meta.mkString(", ") + "}")
}

trait K {

}

trait Context {
  val productions: Map[String, Set[Production]]
}

case class Sort(name: String)
// MetaKLabel ::= Token{"dummy"}  [regex(`[^`\s]+`)]

case class SimpleKLabel(name: String) extends KLabel {
  //  def apply(params: K*) = KApp(this, params)
}

// MetaKConstant ::= TOKENID "::" SORTID
case class KConstant(label: KLabel) extends KItem {
  val sort = Sort("KItem")
}

object KApp {
  def apply(label: KLabel, klist: Seq[K]): KApp = {
    KApp(label, klist, Sort("???"))
  }
}

// MetaKItem ::= MetaKLabel "(" MetaKList ")"
case class KApp(label: KLabel, klist: Seq[K], sort: Sort) extends KItem

// MetaKList  ::= NeList{MetaK,","}
//              | ".::MetaKList"
// is just a List[K]

object Variable extends KLabel {
  val name = "Variable"
}

case class Variable(name: String) extends KItem {
  val label = Variable
  val sort = Sort("Variable")
}

object Hole extends KLabel {
  val name = "Hole"
}

// MetaK ::= NeList{MetaKItem,"~>"}
//         | ".::MetaK"

case class Heated(items: Seq[KItem]) extends K {
  override def toString = items.mkString("⤳")
}

// MetaK ::= MetaK "=>" MetaK
case class Rewrite(left: K, right: K) extends K {
  override def toString = left + " => " + right
}

////////
import Definition._

object Boolean {
  def And = SimpleKLabel("'_andBool_")
  def Or = SimpleKLabel("'_orBool_")
}

case class Cons(sort: Sort) extends KLabel {
  // Cosmin: I don't like this KLabel, but will do for now
  val name = "Cons"
}

case class Cell(name: String) extends KLabel {
}

case class Bag(override val items: K*) extends KItem {
  val label = SimpleKLabel("Bag")
  val sort = Sort("Bag")
}