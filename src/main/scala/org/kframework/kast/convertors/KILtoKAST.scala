package org.kframework.kast.convertors

import org.kframework._
import collection.JavaConversions._
import collection.JavaConverters._
import org.kframework.kast
import kast.outer.Associativity._
import org.kframework.kil.KLabel
import org.kframework.kast.BlandTerm
import org.kframework.kast.BlandLabel

object KILtoKAST extends Function1[kil.Definition, kast.outer.Definition] {

  val Term = kast.SortedTermConstructor(kast.BasicConstructor)

  type Return = Any

  implicit object NoContext extends kast.Context

  def convert(n: kil.Ambiguity): Return = {
    ???
  }

  implicit def convert(n: kil.Attributes): kast.Attributes = {
    kast.Attributes(n map {
      case (_, v: kil.Attribute[Any]) => 
        val sym = Symbol(v.getKey().toString)
        kast.UglyRecorder.ugly += (sym -> v.getKey())
        (sym,
        if (v.getValue() == null)
          true
        else
          v.getValue())
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

  def convert(n: kil.Configuration): kast.outer.Configuration = kast.outer.Configuration(convert(n.getBody()))

  def convert(n: kil.Context): Return = {
    ???
  }

  def convert(n: kil.DataStructureBuiltin): Return = {
    ???
  }

  def apply(n: kil.Definition): kast.outer.Definition = {
    val kilModules = n.getItems().toSet
    kast.outer.Definition(
        Set[kast.outer.Require](),
        kilModules map {
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

  def convert(n: kil.Import): kast.outer.Import = {
    kast.outer.Import(n.getName())
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

  implicit def convert(n: kil.KLabel): kast.Label = n match {
    case l: kil.KLabelConstant => kast.KConstant(l.getLabel(), convert(l.getAttributes()))
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

  def convert(n: kil.Lexical): kast.outer.Lexical = {
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

  def convert(n: kil.LiterateModuleComment): kast.outer.ModuleComment = {
    kast.outer.ModuleComment(n.getValue())
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

  def convert(n: kil.Module): kast.outer.Module = {
    val moduleItems: Seq[kast.outer.Sentence] = (n.getItems() map {
      case n: kil.Import => convert(n)
      case n: kil.Syntax => convert(n)
      case n: kil.Rule => convert(n)
      case n: kil.Configuration => convert(n)
      case n: kil.LiterateModuleComment => convert(n)
      case n: kil.PriorityExtended => convert(n)
    })
    kast.outer.Module(n.getName(), moduleItems.toSet)
  }

  def convert(n: kil.NonTerminal): kast.outer.NonTerminal = {
    kast.outer.NonTerminal(convert(n.getSort()))
  }

  def convert(n: kil.ParseError): Return = {
    ???
  }

  def convert(n: kil.PriorityBlock): kast.outer.Block = {
    import kast.outer.Associativity._
    val assoc = n.getAssoc() match {
      case "left" => Left
      case "right" => Right
      case "non-assoc" => NonAssoc
      case "" => Unspecified
    }
    val productions = n.getProductions() map convert
    kast.outer.Block(assoc, productions.toSet)
  }

  def convert(n: kil.PriorityBlockExtended): Return = {
    ???
  }

  def convert(n: kil.PriorityExtended): kast.outer.SyntaxPriority = {
    kast.outer.SyntaxPriority(n.getPriorityBlocks().map {
      case pb: kil.PriorityBlockExtended => kast.outer.SyntaxPriorityBlock(pb.getProductions() map convert)
    } toSet)
  }

  def convert(n: kil.PriorityExtendedAssoc): Return = {
    ???
  }

  def convert(n: kil.Production): kast.outer.Production = {
    n.getItems().asScala match {
      case Seq(i: kil.UserList) =>
        convert(i); ??? // how to merge attributes?
      case l: Seq[_] => kast.outer.RegularProduction(l map {
        case l: kil.Lexical => convert(l)
        case t: kil.Terminal => convert(t)
        case nt: kil.NonTerminal => convert(nt)
      }, convert(n.getAttributes()))
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

  object ConcreteDataStructureSize extends kast.Key[java.util.Map[kil.Variable, Integer]] {
    val key = 'concreteDataStructureSize
  }

  def convert(n: kil.Rule): kast.outer.Rule = {
    val attributes = convert(n.getAttributes())

    val withRequire = Option(convert(n.getRequires()))
      .fold(kast.Attributes()) { c: kast.Term =>
        attributes + (kast.SymbolicConstraint -> c)
      }
    val withEnsure = Option(convert(n.getEnsures()))
      .fold(kast.Attributes()) { c: kast.Term =>
        attributes + (kast.SymbolicConstraint -> c)
      }
    
    
    val body = convert(n.getBody())

    val bodyWithRequiresEnsures = body match {
      case kast.Rewrite(left, right, attributes) => {
        kast.Rewrite(
          left.copy(attributes = left.attributes ++ withRequire),
          right.copy(attributes = right.attributes ++ withEnsure),
          attributes)
      }
    }

    kast.outer.Rule(n.getLabel(), bodyWithRequiresEnsures, attributes)
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

  def convert(n: kil.Syntax): kast.outer.Syntax = {
    val blocks = n.getPriorityBlocks() map convert
    kast.outer.Syntax(convert(n.getDeclaredSort().getSort()), blocks)
  }

  object RemoveMapEntries extends kast.Key[Map[kast.Term, kast.Term]] {
    val key = 'removeMapEntries
  }
  object UpdateMapEntries extends kast.Key[Map[kast.Term, kast.Term]] {
    val key = 'updateSetEntires
  }
  object RemoveSetEntries extends kast.Key[Set[kast.Term]] {
    val key = 'removeSetEntries
  }
  object RemoveLeftChildren extends kast.Key[Map[kast.Term, kast.Term]] {
    val key = 'removeLeftChildren
  }
  object RemoveRightChildren extends kast.Key[Map[kast.Term, kast.Term]] {
    val key = 'removeRightChildren
  }

  implicit def convert(n: kil.Term): kast.Term = n match {
    // fundamental
    case rw: kil.Rewrite => Term(kast.Rewrite.name, Seq(rw.getLeft(), rw.getRight()) map convert, rw.getSort(), rw.getAttributes())
    case kseq: kil.KSequence => Term(kast.KSeq.name, kseq.getContents() map convert, kseq.getSort(), kseq.getAttributes())
    case t: kil.Hole => Term(kast.Hole.name, Seq(), t.getSort())
    case variable: kil.Variable =>
      Term(variable.fullName(), Seq(), variable.getSort(), variable.getAttributes() + kast.Flag('variable))

    // cell
    case cell: kil.Cell =>
      val attributes = convert(cell.getAttributes()) ++ kast.Attributes(cell.getCellAttributes() map { case (k, v) => (Symbol(k), v) } toMap)
      Term(cell.getLabel(),
        Seq(convert(cell.getContents())),
        convert(cell.getSort()),
        attributes + kast.Flag('cell))

    // collections
    case bag: kil.Bag =>
      val collectIn = collection.mutable.ArrayBuffer[kil.Term]()
      org.kframework.kil.Bag.flatten(collectIn, bag.getContents())
      Term("Bag", collectIn map convert, bag.getSort(), bag.getAttributes() + kast.Flag('bag))
      
    case map: kil.MapBuiltin =>
      val pairs = (map.elements() map {
        case (_1, _2) => 
          BlandTerm(BlandLabel(map.sort().elementLabel()), Seq(_1, _2), kast.Attributes())
      }).foldLeft(BlandTerm(BlandLabel(map.sort().unitLabel()), Seq(), kast.Attributes())) {
        case (a, b) => BlandTerm(BlandLabel(map.sort().constructorLabel()), Seq(a, b), kast.Attributes())
      }
    ???
//      Term("Map", pairs, map.getSort(), map.getAttributes() + kast.Flag('map))

    // kapp 
    case kapp: kil.KApp => Term(kapp.getLabel().toString(), convert(kapp.getChild().asInstanceOf[kil.KList]), kapp.getSort(), kapp.getAttributes())

    // other stuff...
    case mapUpdate: kil.MapUpdate =>
      val removeEntries: Map[kast.Term, kast.Term] = mapUpdate.removeEntries() map { case (k, v) => (convert(k), convert(v)) } toMap
      val updateEntries: Map[kast.Term, kast.Term] = mapUpdate.updateEntries() map { case (k, v) => (convert(k), convert(v)) } toMap
      val variable: kast.Variable = convert(mapUpdate.map())

      Term("MapUpdate", Seq(variable), mapUpdate.getSort(),
        convert(mapUpdate.getAttributes()) +
          (RemoveMapEntries -> removeEntries) + (UpdateMapEntries -> updateEntries))

    // other stuff...
    case setUpdate: kil.SetUpdate =>
      val removeEntries: Set[kast.Term] = setUpdate.removeEntries() map convert toSet

      val variable: kast.Variable = convert(setUpdate.set())

      Term("SetUpdate", Seq(variable), setUpdate.getSort(),
        convert(setUpdate.getAttributes()) +
          (RemoveSetEntries -> removeEntries))

    case listUpdate: kil.ListUpdate =>
      val variable = convert(listUpdate.base())
      val leftChildren = listUpdate.getChildren(kil.ListUpdate.ListChildren.REMOVE_LEFT)
      val rightChildren = listUpdate.getChildren(kil.ListUpdate.ListChildren.REMOVE_RIGHT)

      Term("ListUpdate", Seq(variable), listUpdate.getSort(),
        convert(listUpdate.getAttributes()) +
          (RemoveLeftChildren -> leftChildren) +
          (RemoveRightChildren -> rightChildren))

    case listBuiltin: kil.ListBuiltin =>
      val baseTerms = listBuiltin.baseTerms() map convert
      val left = listBuiltin.elementsLeft() map convert
      val right = listBuiltin.elementsRight() map convert

      Term("List",
        left ++ baseTerms ++ right,
        listBuiltin.getSort(),
        listBuiltin.getAttributes() + kast.Flag('list))

    case setBuiltin: kil.SetBuiltin =>
      val base = setBuiltin.baseTerms() map convert toSeq
      val elements = setBuiltin.elements() map convert

      Term("Set",
        base ++ elements,
        setBuiltin.getSort(),
        setBuiltin.getAttributes() + kast.Flag('set))

    case termComment: kil.TermComment => Term("TermComment", Seq(), null, kast.Attributes())
      
    case p : kil.KItemProjection => Term("Projection", Seq(convert(p.getTerm())), p.getSort(), p.getAttributes())
     
    // propagating null -- not sure we should
    case null => null
  }

  def convert(n: kil.TermComment): Return = {
    ???
  }

  def convert(n: kil.TermCons): Return = {
    ???
  }

  def convert(n: kil.Terminal): kast.outer.Terminal = kast.outer.Terminal(n.getTerminal())

  def convert(n: kil.Token): Return = {
    ???
  }

  def convert(n: kil.UserList): kast.outer.UserList = {
    kast.outer.UserList(n.getSort(), n.getSeparator(), convert(n.getAttributes()))
  }

  implicit def convert(n: kil.Sort): kast.Sort = kast.Sort(n.getName())

  def convert(n: kil.Variable): kast.Variable = {
    kast.Variable(n.getName(), n.getAttributes() + (kast.Sort -> convert(n.getSort())))
  }
}