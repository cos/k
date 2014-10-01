//package org.kframework.kastPLAY
//
//import org.kframework.kil.Source
//import org.kframework.kil.Location
//
//trait HasAttributes {
//  def attributes: Map[String, String]
//}
//
//trait HasSource {
//  val source: Source
//}
//
//trait HasLocation {
//  val location: Option[Location]
//}
//
//trait HasOrigin extends HasSource with HasLocation {
//  val origin: Origin
//  val source = origin.source
//  val location = origin.location
//}
//
//case class Origin(
//  source: Source,
//  location: Option[Location])
//  extends HasSource with HasLocation
//
//trait HasChildren[T] {
//  def children: List[T]
//}
//
//trait Context
//
//trait ClonableFromChildren[T] extends HasChildren[T] {
//  def cloneWithChildren(newChildren: List[T])(implicit context: Context): T
//}
//
//trait SymbolicConstraint
//
//trait Variable
//
//trait HasVariables[T <: HasVariables[T]] extends HasChildren[T] {
//  val variables: Set[Variable] = children flatMap { _.variables } toSet
//  val isGround = variables.empty
//  val isNotSyntacticallyUnifiable: Boolean
//}
//
//trait HasSubstitution[T <: HasSubstitution[T]] extends HasChildren[T] with ClonableFromChildren[T] {
//  def substitute(substitution: Map[Variable, T])(implicit context: Context): T = {
//    this match {
//      case v: Variable => substitution.get(v).getOrElse(this.asInstanceOf[T])
//      case _ => cloneWithChildren(children map { _.substitute(substitution) })
//    }
//  }
//  def substitute(substitution: PartialFunction[T, T])(implicit context: Context): T = {
//    substitution.lift.apply(this.asInstanceOf[T]).getOrElse(
//      cloneWithChildren(children map { _.substitute(substitution) }))
//  }
//}
//
//trait Sort
//
//trait HasSort {
//  val sort: Sort
//  val isExactSort: Boolean
//}
//
//trait CanBeEvaluated {
//  def evaluate(implicit context: Context): Term = {
//    ???
//    //    Evaluator.evaluate(this, null, context)
//  }
//  def expandPatterns(constraint: SymbolicConstraint, narrow: Boolean)(implicit context: Context): Term = {
//    ???
//    //    PatternExpander.expand(this, constraint, narrow, context);
//  }
//}
//
//trait Term extends HasVariables[Term] with HasSubstitution[Term] with CanBeEvaluated {
//
//}
//
//object Definition {
//  trait Item
//  case class Require(file: String, origin: Origin) extends Item with HasOrigin
//
//  object Module {
//    trait Item
//
//    case class Import(moduleName: String, origin: Origin) extends Item with HasOrigin
//
//    trait Labeled {
//      val label: String
//    }
//
//    case class StringSentence(
//      label: String,
//      origin: Origin,
//      attributes: Map[String, String] = Map()) extends Item with Labeled with HasOrigin with HasAttributes
//
//    trait Sentence extends Item with Labeled with HasOrigin with HasAttributes {
//      val label: String
//      val body: Term
//      val requires: Option[Term]
//      val ensures: Option[Term]
//      val origin: Origin
//      val attributes: Map[String, String]
//    }
//
//    case class Configuration(
//      label: String,
//      body: Term,
//      requires: Option[Term] = None,
//      ensures: Option[Term] = None,
//      origin: Origin,
//      attributes: Map[String, String] = Map()) extends Sentence
//
//    case class Context(
//      label: String,
//      body: Term,
//      requires: Option[Term] = None,
//      ensures: Option[Term] = None,
//      origin: Origin,
//      attributes: Map[String, String] = Map()) extends Sentence
//
//    case class Rule(
//      label: String,
//      body: Term,
//      requires: Option[Term] = None,
//      ensures: Option[Term] = None,
//      origin: Origin,
//      attributes: Map[String, String] = Map()) extends Sentence
//
//    object Production {
//      trait Item
//      case class Lexical(pattern: String)
//      case class NonTerminal(sort: Sort)
//      case class Terminal(terminal: String)
//
//      object ListType extends Enumeration {
//        type ListType = Value
//        val OneOrMore, ZeroOrMore = Value
//      }
//      case class UserList(sort: Sort, separator: String, listType: ListType.ListType)
//    }
//
//    case class Production(items: Production)
//
//    case class PriorityBlock(productions: List[Production])
//
//    case class Syntax(sort: Sort, blocks: List[PriorityBlock])
//  }
//  case class Module(
//    name: String,
//    items: List[Module.Item],
//    origin: Origin) extends Item with HasOrigin
//}
//
//case class Definition(items: List[Definition.Item]) {
//
//}
//
//case class Cell(tag: String, content: Term) extends Term {
//  override def children = List(content)
//  override def cloneWithChildren(children: List[Term])(implicit context: Context): Cell =
//    this.copy(content = children.head)
//  override val isNotSyntacticallyUnifiable = false
//}
//
//trait CachesHashCode {
//  val cachedHashCode = super.hashCode()
//  override def hashCode = cachedHashCode
//}
//
//object Bottom
//object Hole
//
//case class Token(sort: Sort, value: String)