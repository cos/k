package org.kframework.kast

import org.kframework._
import kil.ASTNode
import collection.JavaConversions._
import collection.JavaConverters._
import org.kframework.kast._

object KILtoKAST {
  trait Piggy
  type Return = Any

  def convert(n: kil.Ambiguity): Return = {
    ???
  }

  def convert(n: kil.ASTNode): Return = {
    ???
  }

  def convert(n: kil.Attribute[_]): Return = {
    ???
  }

  def convert(n: kil.Attributes): kast.Definition.Attributes = {
    n map { case (k, v) => (k.toString, v.toString) } toMap
  }

  def convert(n: kil.BackendTerm): Return = {
    ???
  }

  def convert(n: kil.Bag): Return = {
    ???
  }

  def convert(n: kil.BagItem): Return = {
    ???
  }

  def convert(n: kil.BoolBuiltin): Return = {
    ???
  }

  def convert(n: kil.Bracket): Return = {
    ???
  }

  def convert(n: kil.BuiltinLookup): Return = {
    ???
  }

  def convert(n: kil.Cast): Return = {
    ???
  }

  def convert(n: kil.Cell): Return = {
    ???
  }

  def convert(n: kil.Collection): Return = {
    ???
  }

  def convert(n: kil.CollectionBuiltin): Return = {
    ???
  }

  def convert(n: kil.CollectionItem): Return = {
    ???
  }

  def convert(n: kil.Configuration): kast.Configuration = kast.Configuration(n.getBody())

  def convert(n: kil.Constant): Return = {
    ???
  }

  def convert(n: kil.Context): Return = {
    ???
  }

  def convert(n: kil.DataStructureBuiltin): Return = {
    ???
  }

  def convert(n: kil.Definition): kast.Definition = {
    val kilModules = n.getModulesMap().values().toSet
    kast.Definition(kilModules map convert)
  }

  def convert(n: kil.DefinitionItem): Return = {
    ???
  }

  def convert(n: kil.FloatBuiltin): Return = {
    ???
  }

  def convert(n: kil.Freezer): Return = {
    ???
  }

  def convert(n: kil.FreezerHole): Return = {
    ???
  }

  def convert(n: kil.FreezerLabel): Return = {
    ???
  }

  def convert(n: kil.GenericToken): Return = {
    ???
  }

  def convert(n: kil.Hole): Return = {
    ???
  }

  def convert(n: kil.Import): kast.Import = {
    ???
  }

  def convert(n: kil.IntBuiltin): Return = {
    ???
  }

  def convert(n: kil.KApp): Return = {
    ???
  }

  def convert(n: kil.KInjectedLabel): Return = {
    ???
  }

  def convert(n: kil.KItemProjection): Return = {
    ???
  }

  def convert(n: kil.KLabel): Return = {
    ???
  }

  def convert(n: kil.KLabelConstant): Return = {
    ???
  }

  def convert(n: kil.KLabelInjection): Return = {
    ???
  }

  def convert(n: kil.KList): Return = {
    ???
  }

  def convert(n: kil.KSequence): Return = {
    ???
  }

  def convert(n: kil.Lexical): kast.Lexical = {
    ???
  }

  def convert(n: kil.ListBuiltin): Return = {
    ???
  }

  def convert(n: kil.ListLookup): Return = {
    ???
  }

  def convert(n: kil.ListTerminator): Return = {
    ???
  }

  def convert(n: kil.ListUpdate): Return = {
    ???
  }

  def convert(n: kil.LiterateDefinitionComment): Return = {
    ???
  }

  def convert(n: kil.LiterateModuleComment): Return = {
    ???
  }

  def convert(n: kil.MapBuiltin): Return = {
    ???
  }

  def convert(n: kil.MapLookup): Return = {
    ???
  }

  def convert(n: kil.MapUpdate): Return = {
    ???
  }

  def convert(n: kil.Module): kast.Module = {
    val moduleItems: Seq[Sentence] = (n.getItems() map {
      case n: kil.Import => convert(n)
      case n: kil.Syntax => convert(n)
      case n: kil.Rule => convert(n)
      case n: kil.Configuration => convert(n)
    })
    kast.Module(n.getName(), moduleItems.toSet)
  }

  def convert(n: kil.NonTerminal): kast.NonTerminal = {
    kast.NonTerminal(n.getName(), convert(n.getSort()))
  }

  def convert(n: kil.ParseError): Return = {
    ???
  }

  def convert(n: kil.PriorityBlock): kast.Block = {
    import kast.Associativity._
    val assoc = n.getAssoc() match {
      case "left" => Left
      case "right" => Right
      case "non-assoc" => NonAssoc
      case "" => Unspecified
    }
    val productions = n.getProductions() map convert
    kast.Block(assoc, productions.toSet)
  }

  def convert(n: kil.PriorityBlockExtended): Return = {
    ???
  }

  def convert(n: kil.PriorityExtended): Return = {
    ???
  }

  def convert(n: kil.PriorityExtendedAssoc): Return = {
    ???
  }

  def convert(n: kil.Production): kast.Production = {
    n.getItems().asScala match {
      case Seq(i: kil.UserList) =>
        convert(i); ??? // how to merge attributes?
      case l: Seq[_] => NormalProduction(l map {
        case l: kil.Lexical => convert(l)
        case t: kil.Terminal => convert(t)
        case nt: kil.NonTerminal => convert(nt)
      }, AttributesDeclaration(convert(n.getAttributes())))
      case x => println(x.getClass()); ???
    }
  }

  def convert(n: kil.ProductionItem): Return = {
    ???
  }

  def convert(n: kil.ProductionReference): Return = {
    ???
  }

  def convert(n: kil.Require): Return = {
    ???
  }

  def convert(n: kil.Restrictions): Return = {
    ???
  }

  def convert(n: kil.Rewrite): Return = {
    ???
  }

  def convert(n: kil.Rule): kast.Rule = {
    kast.Rule(
      body = convert(n.getBody()),
      requires = Option(convert(n.getRequires())),
      ensures = Option(convert(n.getEnsures())),
      attributes = AttributesDeclaration(convert(n.getAttributes())))
  }

  def convert(n: kil.Sentence): Return = {
    ???
  }

  def convert(n: kil.SetBuiltin): Return = {
    ???
  }

  def convert(n: kil.SetLookup): Return = {
    ???
  }

  def convert(n: kil.SetUpdate): Return = {
    ???
  }

  def convert(n: kil.StringBuiltin): Return = {
    ???
  }

  def convert(n: kil.StringSentence): Return = {
    ???
  }

  def convert(n: kil.Syntax): kast.Syntax = {
    val blocks = n.getPriorityBlocks() map convert
    kast.Syntax(convert(n.getDeclaredSort().getSort()), blocks)
  }

  implicit def convert(n: kil.Term): kast.K = n match {
    case t: kil.Hole => kast.Hole()
    case rw: kil.Rewrite => kast.Rewrite(rw.getLeft(), rw.getRight())
    case cons: kil.TermCons => kast.Cons(cons.getSort())(cons.getContents() map convert: _*)
    case cell: kil.Cell => kast.Cell(cell.getLabel())(cell.getContents())
    case variable: kil.Variable => kast.Variable(variable.fullName())
    case bag: kil.Bag => kast.Bag(bag.getContents() map convert :_*)
    case null => null
  }

  def convert(n: kil.TermComment): Return = {
    ???
  }

  def convert(n: kil.TermCons): Return = {
    ???
  }

  def convert(n: kil.Terminal): kast.Terminal = kast.Terminal(n.getTerminal())

  def convert(n: kil.Token): Return = {
    ???
  }

  def convert(n: kil.UserList): kast.UserList = {
    kast.UserList(n.getSort(), n.getSeparator(), AttributesDeclaration(convert(n.getAttributes())))
  }

  implicit def convert(n: kil.Sort) = kast.Sort(n.getName())

  def convert(n: kil.Variable): Return = {
    ???
  }
}