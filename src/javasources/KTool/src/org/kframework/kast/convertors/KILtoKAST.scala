package org.kframework.kast.convertors

import org.kframework._
import collection.JavaConversions._
import collection.JavaConverters._
import org.kframework.kast.Associativity.Left
import org.kframework.kast.Associativity.NonAssoc
import org.kframework.kast.Associativity.Right
import org.kframework.kast.Associativity.Unspecified
import org.kframework.kast.Attributes
import org.kframework.kast.Boolean
import org.kframework.kast.Context
import org.kframework.kast.Sentence

object KILtoKAST { //extends Function1[kil.Definition, kast.Definition]

  val Term = kast.SortedTermConstructor(kast.BasicConstructor)

  type Return = Any

  implicit object NoContext extends Context

  def convert(n: kil.Ambiguity): Return = {
    ???
  }

  implicit def convert(n: kil.Attributes): kast.Attributes = {
    kast.Attributes(n map {
      case (_, v) => (Symbol(v.getKey().toString),
        if (v.getValue() == null)
          Attributes.on
        else
          v.getValue().toString)
    } toMap)
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

  def convert(n: kil.Collection): Return = {
    ???
  }

  def convert(n: kil.CollectionBuiltin): Return = {
    ???
  }

  def convert(n: kil.CollectionItem): Return = {
    ???
  }

  def convert(n: kil.Configuration): kast.Configuration = kast.Configuration(convert(n.getBody()))

  def convert(n: kil.Context): Return = {
    ???
  }

  def convert(n: kil.DataStructureBuiltin): Return = {
    ???
  }

  def apply(n: kil.Definition): kast.Definition = {
    val kilModules = n.getItems().toSet
    kast.Definition(kilModules map {
      case m: kil.Module => convert(m)
    })
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

  implicit def convert(n: kil.KLabel): kast.KLabel = n match {
    case l: kil.KLabelConstant => kast.KConstant(l.getLabel(), convert(l.getAttributes()))
  }

  def convert(n: kil.KLabelConstant): Return = {
    ???
  }

  def convert(n: kil.KLabelInjection): Return = {
    ???
  }

  def convert(n: kil.KList): Seq[kast.Term] = {
    n.getContents() map convert
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
      case l: Seq[_] => kast.NormalProduction(l map {
        case l: kil.Lexical => convert(l)
        case t: kil.Terminal => convert(t)
        case nt: kil.NonTerminal => convert(nt)
      }, kast.AttributesDeclaration(convert(n.getAttributes())))
      case x => println(x.getClass()); ???
    }
  }

  def convert(n: kil.ProductionItem): Return = {
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
      requires = Option(convert(n.getRequires())).getOrElse(Boolean.True),
      ensures = Option(convert(n.getEnsures())).getOrElse(Boolean.True),
      attributes = kast.AttributesDeclaration(convert(n.getAttributes())))
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

  implicit def convert(n: kil.Term): kast.Term = n match {
    // fundamental
    case rw: kil.Rewrite => Term("=>", Seq(rw.getLeft(), rw.getRight()) map convert, rw.getSort(), rw.getAttributes())
    case kseq: kil.KSequence => Term("⤳", kseq.getContents() map convert, kseq.getSort(), kseq.getAttributes())
    case t: kil.Hole => Term("HOLE", Seq(), t.getSort())
    case variable: kil.Variable =>
      Term(variable.fullName(), Seq(), variable.getSort(), variable.getAttributes() + 'variable)

    // cell
    case cell: kil.Cell =>
      val attributes = convert(cell.getAttributes()) ++ Attributes(cell.getCellAttributes() map { case (k, v) => (Symbol(k), v) } toMap)
      Term(cell.getLabel(),
        Seq(convert(cell.getContents())),
        convert(cell.getSort()),
        attributes + 'cell)

    // collections
    case bag: kil.Bag => Term("Bag", bag.getContents() map convert, bag.getSort(), bag.getAttributes() + 'bag)

    // kapp 
    case kapp: kil.KApp => Term(kapp.getLabel().toString(), convert(kapp.getChild().asInstanceOf[kil.KList]), kapp.getSort(), kapp.getAttributes())

    // propagating null -- not sure we should
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
    kast.UserList(n.getSort(), n.getSeparator(), kast.AttributesDeclaration(convert(n.getAttributes())))
  }

  implicit def convert(n: kil.Sort): kast.Sort = kast.Sort(n.getName())

  def convert(n: kil.Variable): Return = {
    ???
  }
}