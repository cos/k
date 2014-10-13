package org.kframework.kore

trait KApplyToString {
  self: KApply =>

  override def toString = {
    val childrenString = if (klist.isEmpty) "" else "(" + klist.sortBy(_.toString).mkString(", ") + ")"

    val cleanAttributes = att.filter({ case KApply(label, _, _) => label == KLabel("sort") })
    val attributesString = if (cleanAttributes.isEmpty) "" else cleanAttributes

    //    val sortString = att.get(Sort).map(":" + _).getOrElse("")

    klabel + childrenString + attributesString
  }
}