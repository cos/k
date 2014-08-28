package org.kframework.kast.inner

trait Bubble

case class Sort(name: String)

trait KItem

// MetaKLabel ::= Token{"dummy"}  [regex(`[^`\s]+`)]
case class KLabel(name: String)

trait KConstant extends KItem

// MetaKConstant ::= TOKENID "::" SORTID
case class BasicKConstant(value: String, sort: Sort) extends KConstant

// MetaKConstant ::= #klabel(MetaKLabel)
case class WrappedKLabel(label: KLabel) extends KConstant

// MetaKItem ::= MetaKLabel "(" MetaKList ")"
case class KApp(label: KLabel, klist: List[K]) extends KItem

// MetaKList  ::= NeList{MetaK,","}
//              | ".::MetaKList"
// is just a List[K]

case class Variable(name: String) extends KItem

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
