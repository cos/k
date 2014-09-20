package org.kframework.kast.convertors

import org.kframework.backend.java._
import org.kframework.{ kil => frontkil }
import org.kframework.kil.loader.Context
import org.kframework.backend.java.indexing.IndexingTable
import org.kframework.backend.java.kil.GlobalContext
import org.kframework.backend.java.kil.KLabelConstant
import org.kframework.backend.java.symbolic.UninterpretedConstraint
import collection.JavaConversions._
import org.kframework.kast
import org.kframework.backend.java.symbolic.MacroExpander
import org.kframework.backend.java.kil.TermContext
import org.kframework.backend.java.symbolic.KILtoBackendJavaKILTransformer
import com.google.common.collect.ArrayListMultimap
import com.google.common.collect.Lists
import org.kframework.backend.java.builtins.BoolToken

case class KASTtoBackendKIL(globalContext: GlobalContext, context: Context, indexingData: IndexingTable.Data) extends Function1[kast.outer.Definition, kil.Definition] {
  def apply(d: kast.outer.Definition): kil.Definition = {
    val definition = new kil.Definition(context, indexingData)
    globalContext.setDefinition(definition)

    assert(d.modules.size == 1)

    val singletonModule = d.modules.head
    singletonModule.sentences foreach {
      case r: kast.outer.Rule if !r.attributes(kast.Flag(Symbol(org.kframework.kil.Attribute.PREDICATE_KEY))) => definition.addRule(apply(r))
      case _ =>
    }
    singletonModule.sentences collect {
      case kast.outer.Syntax(_, blocks, _) => blocks flatMap {
        _.productions map { _.klabel }
      }
    }

    definition
  }

  //  commented the code below because it is left untranslated in KILTOBackendKILTransformer
  //  def postProcessing(d: kil.Definition): kil.Definition = {
  //    globalContext.setDefinition(d);
  //
  //    val expandedDefinition = new MacroExpander(TermContext.of(globalContext)).processDefinition();
  //    globalContext.setDefinition(expandedDefinition);
  //
  //    val evaluatedDefinition = KILtoBackendJavaKILTransformer.evaluateDefinition(globalContext);
  //    globalContext.setDefinition(evaluatedDefinition);
  //    evaluatedDefinition;
  //  }

  def apply(r: kast.outer.Rule): kil.Rule = {
    val (leftHandSide, rightHandSide) = r.body match {
      case kast.Rewrite(left, right, attributes) =>
        (apply(left), apply(right))
    }

    //    val concreteCollectionSize = r.attributes(KILtoKAST.ConcreteDataStructureSize)
    val concreteCollectionSize: java.util.Map[frontkil.Variable, Integer] = null

    import org.kframework.kil.KLabelConstant._
    val andLabel1 = ANDBOOL_KLABEL.getLabel()
    val andLabel2 = BOOL_ANDBOOL_KLABEL.getLabel()

    import kast.HasSymbolicConstraint.TermHasSymbolicConstraint

    val (requires, ensures) = r.body match {
      case kast.Rewrite(left, right, _) => (left.constraint, right.constraint)
    }

    val requiresKIL = (requires match {
      case kast.Term(kast.Label(`andLabel1` | `andLabel2`), klist, _) => klist
      case t => Seq(t)
    }) map apply

    val ensuresKIL = (ensures match {
      case kast.Term(kast.Label(`andLabel1` | `andLabel2`), klist, _) => klist
      case t => Seq(t)
    }) map apply

    object BackendRuleData extends kast.Key[kil.JavaBackendRuleData] {
      val key = Symbol("@org.kframework.backend.java.kil.JavaBackendRuleData")
    }

    val lookupsBuilder = UninterpretedConstraint.builder()
    val backendRuleData = r.attributes(BackendRuleData)

    backendRuleData.getLookups() foreach { lookup =>
      val base = oldVisit(lookup.base(), concreteCollectionSize)
      val key = apply(KILtoKAST.convert(lookup.key()))

      val kind = lookup.kind() match {
        case frontkil.Sort.KITEM => kil.Kind.KITEM
        case frontkil.Sort.K => kil.Kind.K
        case frontkil.Sort.KLIST => kil.Kind.KLIST
        case frontkil.Sort.KLABEL => kil.Kind.KLABEL
        case frontkil.Sort.BAG_ITEM => kil.Kind.CELL
        case frontkil.Sort.BAG => kil.Kind.CELL_COLLECTION
      }

      lookup match {
        case l: frontkil.SetLookup =>
          if (lookup.choice()) {
            lookupsBuilder.add(new kil.SetElementChoice(base), key)
          } else {
            lookupsBuilder.add(new kil.SetLookup(base, key), builtins.BoolToken.TRUE)
          }
        case l: frontkil.MapLookup =>
          if (lookup.choice()) {
            lookupsBuilder.add(new kil.MapKeyChoice(base), key);
          } else {
            val value = apply(KILtoKAST.convert(lookup.value()))
            lookupsBuilder.add(new kil.MapLookup(base, key, kind), value);
          }
        case l: frontkil.ListLookup =>
          val value = apply(KILtoKAST.convert(lookup.value()))
          lookupsBuilder.add(new kil.ListLookup(base, key, kind), value);
      }
    }
    val variables = kast.Traversals.collectBF({ case x: kast.Variable => x })(r.body)
    val freshVariables = variables filter { _.label.name.startsWith("!") } map apply

    new kil.Rule(r.label, leftHandSide, rightHandSide, requiresKIL, ensuresKIL,
      freshVariables, lookupsBuilder.build(), false, // removed node.isCompiledForFastRewriting()
      null, null, null, null, apply(r.attributes), globalContext.getDefinition());
  }

  def apply(kastAtt: kast.Attributes): frontkil.Attributes = {
    val kilAtt = new frontkil.Attributes()
    kastAtt.a foreach {
      case (k: Symbol, v: Any) => kilAtt.add(new frontkil.Attribute(kast.UglyRecorder.ugly(k), v))
    }
    kilAtt
  }

  def apply(r: kast.Term): kil.Term = r match {
    case r: kast.Variable => apply(r)
    case c: kast.Cell => apply(c)
    case seq: kast.KSeq => apply(seq)
    case list: kast.BuiltinList => apply(list)
    case kast.Boolean.True => BoolToken.TRUE
    case kast.BlandTerm(kast.BlandLabel(l), klist, attributes) =>
      kil.KItem.of(
        kil.KLabelConstant.of(l, context),
        new kil.KList(klist map apply, null),
        kil.TermContext.of(globalContext))
  }

  def apply(r: kast.KSeq): kil.KSequence = {
    import kast.HasSort._
    val (variable, items) = r.children match {
      case rest :+ (v @ kast.Variable(_, att)) if att.get(kast.Sort) == Some(kast.Sort("K")) => (apply(v), rest map apply)
      case l => (null, l map apply)
    }

    new kil.KSequence(items, variable)
  }

  def apply(c: kast.Cell): kil.Term = c.content match {
    case b: kast.BuiltinBag => new kil.Cell[kil.CellCollection](kil.CellLabel.of(c.label.name), apply(b))
    case c: kast.Cell => ???
    case t: kast.Term =>
      val content = apply(t)
      new kil.Cell(kil.CellLabel.of(c.label.name), content)
  }

  def apply(b: kast.BuiltinBag): kil.CellCollection = {
    import kast.HasSort._

    val cells = ArrayListMultimap.create[kil.CellLabel, kil.Cell[_ <: kil.Term]]();
    val baseTerms = Lists.newArrayList[kil.Variable]();
    b.children foreach {
      case kast.Term(kast.BlandLabel("TermComment"), _, _) => 
      case c: kast.Cell =>
        val kilC = apply(c).asInstanceOf[kil.Cell[_ <: kil.Term]]
        cells.put(kilC.getLabel(), kilC)
      case t: kast.Variable if t.sort == kast.Sort("Bag") => baseTerms.add(apply(t))
      case x => apply(x)
    }
    new kil.CellCollection(cells, baseTerms, context)
  }

  /*
   * This doesn't handle left/right
   */
  def apply(l: kast.BuiltinList): frontkil.ASTNode = {
    val builder = kil.BuiltinList.builder()
    l.children foreach {
      c => builder.addItem(apply(c))
    }
    builder.build()
  }

    /*
   * This doesn't handle base
   */
  def apply(m: kast.BuiltinMap): frontkil.ASTNode = {
    val builder = kil.BuiltinMap.builder()
    m.children foreach {
      case kast.BuiltinTuple2(key,value) => builder.put(apply(key),apply(value))
    }
    builder.build()
  }
  /*
    
        @Override
    public ASTNode visit(org.kframework.kil.MapBuiltin node, Void _)  {
        BuiltinMap.Builder builder = BuiltinMap.builder();
        for (Map.Entry<org.kframework.kil.Term, org.kframework.kil.Term> entry :
                node.elements().entrySet()) {
            builder.put(
                    (Term) this.visitNode(entry.getKey()),
                    (Term) this.visitNode(entry.getValue()));
        }
        for (org.kframework.kil.Term term : node.baseTerms()) {
            builder.concatenate((Term) this.visitNode(term));
        }
        return builder.build();
    }
   * 
   * 
   */

  def apply(r: kast.Variable): kil.Variable = {
    import kast.HasSort._
    val kilSort = frontkil.Sort.of(r.sort.name)
    kilSort match {
      case frontkil.Sort.BAG => new kil.Variable(r.name, kil.Kind.CELL_COLLECTION.asSort())
      case frontkil.Sort.K => new kil.Variable(r.name, kil.Sort.KSEQUENCE)
      case frontkil.Sort.KLIST => new kil.Variable(r.name, kil.Sort.KLIST)
      case s =>
        val sort = Option(context.dataStructureSortOf(s)).map(_.sort() match {
          case frontkil.Sort.LIST => kil.Sort.LIST
          case frontkil.Sort.MAP => kil.Sort.MAP
          case frontkil.Sort.SET => kil.Sort.SET
          case anythingElse => throw new RuntimeException("unexpected data structure " + anythingElse)
        }).getOrElse(kil.Sort.of(s))
        //TODO missing the concreteCollectionSize stuff here
        new kil.Variable(r.name, sort)
    }

  }

  def oldVisit(node: frontkil.Variable, concreteCollectionSize: java.util.Map[frontkil.Variable, Integer]) = {
    import kil._
    val name = node.fullName();

    node.getSort() match {
      case frontkil.Sort.BAG => new Variable(name, Kind.CELL_COLLECTION.asSort())
      case org.kframework.kil.Sort.K => new Variable(name, Sort.KSEQUENCE)
      case org.kframework.kil.Sort.KLIST => new Variable(name, Sort.KLIST)
      case _ => {
        val dataStructureSort = context.dataStructureSortOf(node.getSort());
        if (dataStructureSort != null) {
          val sort =
            if (dataStructureSort.sort().equals(org.kframework.kil.Sort.LIST)) {
              Sort.LIST;
            } else if (dataStructureSort.sort().equals(org.kframework.kil.Sort.MAP)) {
              Sort.MAP;
            } else if (dataStructureSort.sort().equals(org.kframework.kil.Sort.SET)) {
              Sort.SET;
            } else {
              throw new RuntimeException("unexpected data structure " + dataStructureSort.sort());
            }

          //          if (concreteCollectionSize.containsKey(node)) {
          //            new ConcreteCollectionVariable(
          //              name,
          //              sort,
          //              concreteCollectionSize.get(node));
          //          } else {
          new Variable(name, sort);
          //          }
        } else
          new Variable(name, Sort.of(node.getSort()));
      }
    }
  }
}