package org.kframework.backend.java

import collection._
import org.kframework._
import kore._
import KORE._

trait Context {
  val productionForLabel: Map[String, Set[outer.SyntaxProduction]]
}

class KLabel(val name: String)(implicit context: Context) extends kore.KLabel {
  import context._

  lazy val productions = productionForLabel(name)

  lazy val isSortPredicate = name.startsWith("is")
  lazy val predicateSort = if (isSortPredicate) Sort(name.replace("is", "")) else None
  lazy val isFunction = isSortPredicate || (productions exists { _.att.contains("function") })
  lazy val isPattern = productions exists { _.att.contains("pattern") }

  lazy val smtLib: Option[String] = productions map { _.att.get("smtlib") } reduce {
    (a, b) => if (a == b) a else throw new RuntimeException("Different patterns")
  } match {
    case KToken(StringToken, v, _) => Some(v.s)
    case KList() => None
    case _ => throw new RuntimeException("Wront smtlib param")
  }

  override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]

  lazy val cachedHashCode: Int = super.hashCode()
  override def hashCode() = cachedHashCode
  def readResolve(): Object = KLabel(this.name)
}

object KLabel {
  val cache = mutable.Map[String, KLabel]()

  def apply(name: String)(implicit context: Context) = cache.getOrElseUpdate(name, new KLabel(name))
}
