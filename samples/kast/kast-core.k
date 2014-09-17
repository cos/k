// Copyright (c) 2014 K Team. All Rights Reserved.

/* This defines a core subset of K and KAST, with the property that any
KAST definition, and thus any K definition, can be mechanically translated
to a definition using only this minimal subset.  An additional property of
this core is that we do not want to lose anything from the original
definition.  For example, we do not want to lose the capability to pretty
print terms based on the original syntax, or the capability to report error
messages referring to constructs in the original definition.  */


module K-SORT
  syntax K [hook(...)]
endmodule


module OUTER [ hook-base(org.kframework.kast.outer) ]
  import K-SORT

  syntax KDefinition   ::= KRequireList KModuleList [ hook(Definition) ]

  syntax KRequireList  ::= KRequire 
  syntax KRequireList  ::= "" [unit]
  syntax KRequireList  ::= KRequireList KRequireList [assoc list]
  syntax KRequire      ::= "require" KString [ hook(Require) ]

  syntax KModuleList   ::= KModule
  syntax KModuleList   ::= "" [unit]
  syntax KModuleList   ::= KModuleList KModuleList [assoc list]
  syntax KModule       ::= "module" KModuleName KImportList KSentenceList "endmodule" [ hook(Module) ]

  syntax KImportList   ::= KImport
  syntax KImportList   ::= "" [unit]
  syntax KImportList   ::= KImportList KImportList [assoc list]
  syntax KImport       ::= "import" KModuleName                                       [ hook(Import) ]

  syntax KSentenceList ::= KSentence
  syntax KSentenceList ::= "" [unit]
  syntax KSentenceList ::= KSentenceList KSentenceList [assoc list]

  syntax KSentence ::= "syntax" KSort                         "[" KAttributeList "]" [ hook(Syntax) ]
  syntax KSentence ::= "syntax" KSort "::=" KProduction       "[" KAttributeList "]" [ hook(Syntax) ] // missing associativity!
  syntax KSentence ::= "configuration" K                      "[" KAttributeList "]" [ hook(Configuration) ]
  syntax KSentence ::= "rule"          K requires K ensures K "[" KAttributeList "]"  // choice // different from the regular KAST!
  syntax KSentence ::= "context"       K requires K           "[" KAttributeList "]"  // choice

  syntax KProduction ::= KProductionItemList    [ hook(RegularProduction) ] // needs a KAttributeList
  syntax KProduction ::= KTag "(" KSortList ")" [ hook(UserList) ]          // hook(UserList) functional notation
  syntax KProduction ::= "Token{" KRegEx "}"       // token ; Cosmin: Radu wants to allow tokens as KProductionItems

  syntax KProductionItemList ::= KProductionItem
  syntax KProductionItemList ::= "" [unit]
  syntax KProductionItemList ::= KProductionItemList KProductionItemList [assoc list]
  syntax KProductionItem     ::= KSort   [ hook(NonTerminal) ] // non-terminal
  syntax KProductionItem     ::= KString [ hook(Terminal) ] // terminals

  syntax KAttributeList ::= KAttribute
  syntax KAttributeList ::= "" [unit]
  syntax KAttributeList ::= KAttributeList "," KAttributeList  [assoc list]
  syntax KAttribute ::= KTag
  syntax KAttribute ::= KString
  syntax KAttribute ::= KTag "(" KTagList ")"

  syntax KSortList ::= List{KSort,","}
  syntax KTagList ::= List{KTag,","}

  // The following can still change
  syntax KModuleName ::= ...
  syntax KString ::= ...
  syntax KSort ::= ...
  syntax KTag ::= ...
endmodule


module KAST
  import K-SORT

  syntax MetaKLabel    ::= Token{...}
  syntax MetaKConstant ::= #token(KString, KString)
  syntax MetaKConstant ::= #klabel(MetaKLabel)

  syntax MetaKItem     ::= MetaKConstant
  syntax MetaKItem     ::= MetaKLabel "(" MetaKList ")"
  syntax MetaKList     ::= MetaK
  syntax MetaKList     ::= MetaKList "," MetaKList  [assoc unit(`.::KList`)]
  syntax MetaKList     ::= ".::KList"
  syntax MetaK         ::= MetaKItem
  syntax MetaK         ::= MetaK "~>" MetaK  [assoc unit(`.::K`)]
  syntax MetaK         ::= ".::K"

  syntax KVariable     ::= Token{...}
  syntax MetaKItem     ::= KVariable
  syntax MetaKItem     ::= MetaKItem ":" KSort
  syntax MetaK         ::= MetaK "=>" MetaK
  syntax MetaK         ::= "`" MetaK "`"  [bracket]
endmodule


module OUTER-WITH-KAST
  import OUTER
  import KAST
...
endmodule
