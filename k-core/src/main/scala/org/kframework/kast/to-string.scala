package org.kframework.kast

trait TermToString {
  self: Term =>
    
  override def toString = {
    val childrenString = if (children.isEmpty) "" else "(" + children.toList.sortBy(_.toString).mkString(", ") + ")"

    val cleanAttributes = attributes.a.filter({ case (k, _) => k != 'sort })
    val attributesString = if (cleanAttributes.isEmpty) "" else cleanAttributes

    val sortString = attributes.get(Sort).map(":" + _).getOrElse("")

    label.name + childrenString + sortString + attributesString
  }
}