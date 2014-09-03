//package org.kframework.kast.inner
//
//import org.kframework.backend.java.symbolic.Transformer
//import collection.JavaConversions._
//
//object BackendKILtoKASTTransformer {
//  import org.kframework.backend.java.kil._
//  import org.kframework.backend.java.builtins._
//  import org.kframework.backend.java.symbolic.SymbolicConstraint
//  import org.kframework.backend.java.symbolic.UninterpretedConstraint
//
//  import org.kframework.kast.outer
//
//  def apply(d: Definition) = outer.Definition(outer.Module(
//    "FLATTENED",
//    Set(),
//    (d.rules() map transform).toSet))
//
//  def transform(bitVector: BitVector[_]) = ???
//  def transform(boolToken: BoolToken) = ???
//  def transform(builtinList: BuiltinList) = ???
//  def transform(builtinMap: BuiltinMap) = ???
//  def transform(builtinSet: BuiltinSet) = ???
//  def transform(builtinMgu: BuiltinMgu) = ???
//  def transform(cell: Cell[_]) = ???
//  def transform(cellCollection: CellCollection) = ???
//  def transform(collection: Collection) = ???
//  def transform(constrainedTerm: ConstrainedTerm) = ???
//  def transform(hole: Hole) = ???
//  def transform(intToken: IntToken) = ???
//  def transform(kLabelConstant: KLabelConstant) = ???
//  def transform(kLabelFreezer: KLabelFreezer) = ???
//  def transform(kLabelInjection: KLabelInjection) = ???
//  def transform(kItemProjection: KItemProjection) = ???
//  def transform(kItem: KItem) = ???
//  def transform(kCollection: KCollection) = ???
//  def transform(kLabel: KLabel) = ???
//  def transform(kList: KList) = ???
//  def transform(kSequence: KSequence) = ???
//  def transform(listLookup: ListLookup) = ???
//  def transform(listUpdate: ListUpdate) = ???
//  def transform(mapKeyChoice: MapKeyChoice) = ???
//  def transform(mapLookup: MapLookup) = ???
//  def transform(mapUpdate: MapUpdate) = ???
//  def transform(metaVariable: MetaVariable) = ???
//
//  def transform(rule: Rule): outer.Rule = ???
////  outer.Rule(
////    transform(rule.leftHandSide()),
////    transform(rule.rightHandSide()),
////    (rule.requires() map transform).toSet.reduce Boolean.And, 
////    transform(rule.getAttributes()))
//
//  def transform(setElementChoice: SetElementChoice) = ???
//  def transform(setLookup: SetLookup) = ???
//  def transform(setUpdate: SetUpdate) = ???
//  def transform(symbolicConstraint: SymbolicConstraint) = ???
//  def transform(stringToken: StringToken) = ???
//  def transform(node: Term): K = ???
//  def transform(token: Token) = ???
//  def transform(uninterpretedConstraint: UninterpretedConstraint) = ???
//  def transform(uninterpretedToken: UninterpretedToken) = ???
//  def transform(variable: Variable) = ???
//}