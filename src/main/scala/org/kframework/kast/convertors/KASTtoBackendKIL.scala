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

case class KASTtoBackendKIL(globalContext: GlobalContext, context: Context, indexingData: IndexingTable.Data) extends Function1[kast.Definition, kil.Definition] {
  def apply(d: kast.Definition): kil.Definition = {
    val definition = new kil.Definition(context, indexingData)
    globalContext.setDefinition(definition)

    assert(d.modules.size == 1)

    val singletonModule = d.modules.head
    singletonModule.sentences foreach {
      case r: kast.Rule if !r.attributes(kast.Flag(Symbol(org.kframework.kil.Attribute.PREDICATE_KEY))) => definition.addRule(apply(r))
      case _ =>
    }
    singletonModule.sentences collect {
      case kast.Syntax(_, blocks) => blocks flatMap {
        _.productions map { _.getKLabel }
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

  def apply(r: kast.Rule): kil.Rule = {
    val (leftHandSide, rightHandSide) = r.body match {
      case kast.Rewrite(left, right, attributes) =>
        (apply(left), apply(right))
    }

    val concreteCollectionSize = r.attributes(KILtoKAST.ConcreteDataStructureSize)

    import org.kframework.kil.KLabelConstant._
    val andLabel1 = ANDBOOL_KLABEL.getLabel()
    val andLabel2 = BOOL_ANDBOOL_KLABEL.getLabel()
    val requires = (r.requires match {
      case kast.Term(kast.KLabel(`andLabel1` | `andLabel2`), klist, _) => klist
      case t => Seq(t)
    }) map apply

    val ensures = (r.ensures match {
      case kast.Term(kast.KLabel(`andLabel1` | `andLabel2`), klist, _) => klist
      case t => Seq(t)
    }) map apply

    val lookupsBuilder = UninterpretedConstraint.builder()
    val lookups = r.attributes(KILtoKAST.Lookups)
    lookups foreach { lookup =>
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
    val freshVariables = variables filter { _.klabel.name.startsWith("!") } map apply

    new kil.Rule(r.label, leftHandSide, rightHandSide, requires, ensures,
      freshVariables, lookupsBuilder.build(), false, // removed node.isCompiledForFastRewriting()
      null, null, null, null, null, globalContext.getDefinition());
  }

  def apply(r: kast.Term): kil.Term = r match {
    case r: kast.Variable => apply(r)
  }

  def apply(r: kast.Variable): kil.Variable = {
    ???
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

          if (concreteCollectionSize.containsKey(node)) {
            new ConcreteCollectionVariable(
              name,
              sort,
              concreteCollectionSize.get(node));
          } else {
            new Variable(name, sort);
          }
        } else
          new Variable(name, Sort.of(node.getSort()));
      }
    }
  }
}