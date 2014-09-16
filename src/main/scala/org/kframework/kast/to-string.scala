package org.kframework.kast

trait TermToString {
  self: Term =>
    
  override def toString = {
    val klistString = if (klist.isEmpty) "" else "(" + klist.toList.sortBy(_.toString).mkString(", ") + ")"

    val cleanAttributes = (Attributes(attributes.a.filter({ case (k, _) => k != 'sort })))
    val attributesString = if (cleanAttributes.isEmpty) "" else cleanAttributes

    val sortString = attributes.get(Sort).map(":" + _).getOrElse("")

    klabel.name + klistString + sortString + attributesString
  }
}