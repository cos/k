package org.kframework.kast.inner

trait Production

trait KLabel {
  val name: String
}
trait KItem {
  val label: KLabel
  val items: Seq[K] = Seq()
  val meta: Map[Symbol, Any] = Map()
  val sort: Sort
}

trait Context {
  val productions: Map[String, Production]
}

case class Sort(name: String)
// MetaKLabel ::= Token{"dummy"}  [regex(`[^`\s]+`)]

case class SimpleKLabel(name: String) extends KLabel {
  def apply(params: K*) = KApp(this, params)
}

// MetaKConstant ::= TOKENID "::" SORTID
case class KConstant(label: KLabel) extends KItem {
  val sort = Sort("KItem")
}

// MetaKItem ::= MetaKLabel "(" MetaKList ")"
case class KApp(label: KLabel, klist: Seq[K]) extends KItem

// MetaKList  ::= NeList{MetaK,","}
//              | ".::MetaKList"
// is just a List[K]

object Variable extends KLabel {
  val name = "Variable" 
}

case class Variable(name: String) extends KItem {
  val label = Variable
}

trait K

// MetaK ::= NeList{MetaKItem,"~>"}
//         | ".::MetaK"
case class Heated(items: List[K]) extends K

// MetaK ::= MetaK "=>" MetaK
case class Rewrite(left: K, right: K) extends K

object Ellipses extends Enumeration {
  val None, Left, Right, Both = Value
}

class CellSet(cells: Set[Cell]) extends K

case class Cell(
  name: String,
  attributes: Map[String, String],
  content: K,
  ellipses: Ellipses.Value)

////////

object Boolean {
  def And = SimpleKLabel("'_andBool_")
  def Or = SimpleKLabel("'_orBool_")
}
