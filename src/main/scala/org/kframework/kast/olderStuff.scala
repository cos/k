import org.kframework.kast.Sort

// old stuff:

// object Ellipses extends Enumeration {
//  val None, Left, Right, Both = Value
//}

//class CellSet(cells: Set[Cell]) extends K
//
//case class Cell(
//  name: String,
//  attributes: Attributes,
//  content: K,
//  ellipses: Ellipses.Value) extends KLabel

object DirectrTranslationSyntax {

  trait Bubble

  // Definition ::= Requires Modules
  case class Definition(requires: List[Require], modules: List[Module])

  // Require    ::= "require" STRING
  case class Require(name: String)

  // Module     ::= "module" MODULENAME Imports Sentences "endmodule"
  case class Module(name: String,
    imports: List[Import], sentences: List[Sentence])

  // Import     ::= "imports" MODULENAME
  case class Import(name: String)

  // Keyword ::= "rule"
  //                 | ""
  //                 | "configuration"
  //                 | "endmodule"
  //                 | "context"

  trait Sentence

  //  Sentence ::= "rule" Bubble                   Attributes
  //             | "rule" Bubble "requires" Bubble Attributes
  case class Rule(
    bubble: Bubble,
    requires: Option[Bubble],
    attributes: List[Attribute]) extends Sentence

  case class Configuration(bubble: Bubble) extends Sentence
  case class Context(bubble: Bubble) extends Sentence

  // Syntax declarations
  // Syntax ::= "syntax" SORTID "::=" PriorityBlockList
  // Syntax ::= "syntax" SORTID
  // Syntax ::= "syntax" SORTID "[" AnnotationList "]"
  case class Syntax(
    sort: Sort, blocks: List[PriorityBlock],
    attributes: List[Attribute])

  // PriorityBlockList ::= NeList{PriorityBlock,">"}
  // PriorityBlock ::=              ProductionList
  //                       | "left:"      ProductionList
  //                       | "right:"     ProductionList
  //                       | "non-assoc:" ProductionList
  object Associativity extends Enumeration {
    val Left, Right, NonAssoc = Value
  }
  case class PriorityBlock(
    assoc: Associativity.Value,
    productions: List[Production])

  // ProductionList   ::= NeList{Production,"|"}
  // Production       ::= SimpleProduction Attributes
  // SimpleProduction ::= NeList{ProductionItem,""}
  //                          | TAG "(" SORTIDs ")"
  // Attributes ::= "" [onlyLabel, klabel(noAttributes)]
  //                    | "[" AnnotationList "]"
  //
  // ProductionItem ::= SORTID  // non-terminal
  //                        | STRING  // terminals
  //                        | "Token{" TOKEN "}" // will see
  //                        | "List{" SORTID "," STRING "}"
  //                        | "NeList{" SORTID "," STRING "}"

  trait Production // TODO

  trait Attribute // TODO

  //
  // AnnotationList ::= NeList{Annotation,","}
  // Annotation ::= TAG
  //                    | TAG "(" TAGCONTENT ")"
  //                    | TAG "(" STRING ")"
  // TAG        ::= Token{"dummy"} [regex([a-z][A-Za-z\-0-9]*)]
  // TAGCONTENT ::= List{TAGC,""}  // anything with balanced parenthesis. Will have to get original string.
  // TAGC       ::= Token{"dummy"} [regex("[^\\n\\r\\(\\)\\\"]+")]
  // TAGC       ::= "(" TAGCONTENT ")"
  //// STRING     ::= Token{"dummy"} [regex("[\\\"](([^\\\"\\n\\r\\\\])|([\\\\][nrtf\\\"\\\\])|)*[\\\"]")]
  // TOKEN      ::= Token{"dummy"} [regex(([^\}\n\r]|[\\][\}])+)]
  //// SORTID     ::= Token{"dummy"} [regex([#]?[A-Z][A-Za-z0-9]*)]
  // SORTIDs    ::= NeList{SORTID,","}

}