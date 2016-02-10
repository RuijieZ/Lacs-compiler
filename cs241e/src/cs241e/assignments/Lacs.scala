package cs241e.assignments

import Scanning._
import Parsing._
import Typer._
import CodeGenerator._
import Transformations._
import MemoryManagement._

import cs241e.nosource.ParsingPrivate

import cs241e.scanparse._
import Grammars._
import DFAs._

/** Implementations of the definitions from the Lacs language specification. */

object Lacs {

  /** The set of keywords defined in the Lacs language specification. */
  val keywords = Set("def", "var", "Int", "if", "else")

  /** The set of single-character tokens in the Lacs language specification mapped to textual names.
    *
    * You may change the textual names if you wish, but you shouldn't need to.
    */
  val symbols = Map(
    ' ' -> "WHITESPACE",
    '\t' -> "WHITESPACE",
    '\n' -> "WHITESPACE",
    '0' -> "ZERO",
    '<' -> "LT",
    '>' -> "GT",
    '=' -> "BECOMES",
    '+' -> "PLUS",
    '-' -> "MINUS",
    '*' -> "STAR",
    '/' -> "SLASH",
    '%' -> "PCT",
    '(' -> "LPAREN",
    ')' -> "RPAREN",
    '{' -> "LBRACE",
    '}' -> "RBRACE",
    ',' -> "COMMA",
    ';' -> "SEMI",
    ':' -> "COLON"
  )

    /** A DFA that recognizes any valid Lacs token from the list given in the Lacs specification,
      * as well as one of the four whitespace sequences from the specification.
      * The alphabet consists of every character that may appear in any token including whitespace tokens.
      */

  val dfa = {
    
    DFA(
      alphabet = "<>=+-*/%(){},;:! \t\n".toSet ++ ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9'),
      states = Set("start","ID","NUM","SLASH","COMMENT","WHITESPACE","d","i","e","I","v","de",
      "IF","DEF","el","els","ELSE","In","INT","!","NE","ZERO","VAR","va","EQ","BECOMES","ARROW","LT","GT","LE","GE","PLUS","MINUS","STAR","PCT","LPAREN","RPAREN","LBRACE","RBRACE","COMMA","SEMI","COLON"),
      start = "start",
      accepting = Set("NUM","ZERO","COMMENT","WHITESPACE","SLASH", "ID","NE", "INT", "ELSE","DEF","IF","VAR","EQ","BECOMES","ARROW","LT","GT","LE","GE",
      "I","i","In","el","els","d","de","e","v", "va", "PLUS","MINUS","STAR","PCT","LPAREN","RPAREN","LBRACE","RBRACE","COMMA","SEMI","COLON"),
      transition = {
        //deal with the "0" state
        case("start", '0') => "ZERO"
        //deal with NUM
        case("start", '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9') => "NUM"
        case("NUM", '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9') => "NUM"

        //deal with "/"
        case("start",'/') => "SLASH"
        case("SLASH", '/') => "COMMENT"

        //deal with "!" and "!="
        case("start", '!') =>"!"
        case("!", '=') => "NE"

        //deal with comment
        case("COMMENT", x) if "<>=+-*/%(){},;:! \t".toSet ++ ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') contains x => "COMMENT"

        //deal with "space" state
        case("start",' '|'\t'|'\n') => "WHITESPACE"


        // deal with i,if
        case("start", 'i') => "i"
        case("i",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("i",'a'|'b'|'c'|'d'|'e'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("i", '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"



        case("i",'f') => "IF"
        case("IF",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("IF",'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("IF",'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"


        // deal with I, In, Int
        case("start", 'I') => "I"
        case("I",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("I",'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("I", '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"


        case("I",'n') => "In"
        case("In",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("In",'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("In",'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"


        case("In",'t') => "INT"
        case("INT",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("INT",'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("INT",'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"

        // deal with d, de. def
        case("start", 'd') => "d"
        case("d",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("d",'a'|'b'|'c'|'d'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("d", '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"


        case("d",'e') => "de"
        case("de",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("de",'a'|'b'|'c'|'d'|'e'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("de",'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"


        case("de",'f') => "DEF"
        case("DEF",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("DEF",'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("DEF",'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"


        // deal with v,va,var
        case("start", 'v') => "v"
        case("v",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("v",'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("v", '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"


        case("v",'a') => "va"
        case("va",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("va",'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("va",'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"


        case("va",'r') => "VAR"
        case("VAR",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("VAR",'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("VAR",'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"



        // deal with e el els else
        case("start", 'e') => "e"
        case("e",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("e",'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("e", '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"


        case("e",'l') => "el"
        case("el",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("el",'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("el",'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"


        case("el",'s') => "els"
        case("els",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("els",'a'|'b'|'c'|'d'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("els",'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"


        case("els",'e') => "ELSE"
        case("ELSE",'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("ELSE",'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("ELSE",'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"


        // deal with +-*%...
        case("start",'+') => "PLUS"
        case("start",'-') => "MINUS"
        case("start",'*') => "STAR"
        case("start",'%') => "PCT"
        case("start",',') => "COMMA"
        case("start",';') => "SEMI"
        case("start",':') => "COLON"
        case("start",'{') => "LBRACE"
        case("start",'}') => "RBRACE"
        case("start",'(') => "LPAREN"
        case("start",')') => "RPAREN"



        // deal with =
        case("start",'=') => "BECOMES"

        case("BECOMES", '=') => "EQ"
        case("BECOMES", '>') => "ARROW"

        // deal with ==

        // deal with =>


        // deal with ><

        case("start", '<')  => "LT"
        case("start", '>') => "GT"
        case("LT", '=') => "LE"
        case("GT", '=') => "GE"


        // deal with ><=




        // deal with ID
        case("start", 'a'|'b'|'c'|'f'|'g'|'h'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'w'|'x'|'y'|'z') => "ID"
        case("start", 'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("ID", 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z') => "ID"
        case("ID", 'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z') => "ID"
        case("ID",'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')  => "ID"


      }
    )
  }

  /** A scanner for the Lacs programming language. Given an input string, scans it into a sequence of tokens.
    * The kinds of the tokens must correspond to the kinds defined in the Lacs specification
    * (e.g. ID, NUM, LPAREN, ...). White space tokens are removed from the sequence. The resulting
    * sequence is returned with a `Token("BOF")` before it and a `Token("EOF")` after it.
    * If the input string cannot be scanned by the maximal munch algorithm, `error()` is called
    * with a suitable error message.
   */
  def scan(input: String): Seq[Token] =  {
     var seq: Seq[Token] = Seq()
     seq = maximalMunchScan(dfa, input)
     val group1 = Set("ID","DEF","VAR", "INT", "IF", "ELSE", "NUM")
     val group2 = Set("EQ", "NE", "LT", "LE", "GT", "GE", "BECOMES", "ARROW")
     def removeComment(s:Seq[Token]): Seq[Token] = {
       if (s.isEmpty) s
       else if (s.head.kind == "COMMENT" || s.head.kind == "WHITESPACE") removeComment(s.tail)
       else Seq(s.head) ++ removeComment(s.tail)
     }
     def transform(s:Seq[Token]) : Seq[Token] = {
       var temp:Seq[Token] = Seq()
       val set = Set("d","de","v","va","I","In","i","e","el","els")
       for (x <- s.indices) {
         if (set.contains(s(x).kind)) {
           temp = temp :+ Token("ID", s(x).lexeme)
         } else if(s(x).kind == "ZERO") {
           temp = temp :+ Token("NUM", s(x).lexeme)
         } else {
           temp = temp :+ s(x)
         }
       }
       temp
     }
     seq = transform(seq)
     var x = 0
     while (x < seq.length -1) {
        if(group1.contains(seq(x).kind) & group1.contains(seq(x+1).kind)) {
          error("two consecutive same type")
        } else if (group2.contains(seq(x).kind) & group2.contains(seq(x+1).kind)) {
          error("two consecutive same type")
        } else {
          x = x + 1
        }
     }

     seq = removeComment(seq)
     seq = Seq(Token("BOF")) ++ seq ++ Seq(Token("EOF"))
     seq
  }

  /** The grammar for the Lacs programming language copied from the language specification. */
  val grammar = parseGrammar("""
S BOF defdefs EOF
defdefs defdef defdefs
defdefs defdef
defdef DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE
parmsopt parms
parmsopt
parms vardef COMMA parms
parms vardef
vardef ID COLON type
type INT
type LPAREN typesopt RPAREN ARROW type
typesopt types
typesopt
types type COMMA types
types type
vardefsopt VAR vardef SEMI vardefsopt
vardefsopt
defdefsopt defdefs
defdefsopt
expras expra SEMI expras
expras expra
expra ID BECOMES expr
expra expr
expr IF LPAREN test RPAREN LBRACE expras RBRACE ELSE LBRACE expras RBRACE
expr term
expr expr PLUS term
expr expr MINUS term
term factor
term term STAR factor
term term SLASH factor
term term PCT factor
factor ID
factor NUM
factor LPAREN expr RPAREN
factor factor LPAREN argsopt RPAREN
test expr NE expr
test expr LT expr
test expr LE expr
test expr GE expr
test expr GT expr
test expr EQ expr
argsopt args
argsopt
args expr COMMA args
args expr
                             """
  )

  /** Scans and parses a Lacs program, returning the parse tree. */
  def scanAndParse(input: String): Tree = {
    val tokens = scan(input).toIndexedSeq
    val tree = parseCYK(grammar, tokens).getOrElse{
      val longestPrefixKinds = ParsingPrivate.longestPrefix(grammar, tokens)
      val longestPrefixLexemes = tokens.map(_.lexeme).take(longestPrefixKinds.length).mkString(" ")
      error("Parsing error; longest prefix: "+longestPrefixLexemes)
    }
    tree
  }

  /** Scans, parses, and type-checks a Lacs program. Returns the `ProcedureScope`s representing the procedures
    * and a map giving the `Type` of each `Tree` that has one.
    */
  def scanAndParseAndType(input: String): (Seq[ProcedureScope], Map[Tree, Type]) = {
    val tree = scanAndParse(input)
    typeTree(tree)
  }

  /** Type-checks a Lacs program parse tree. Returns the `ProcedureScope`s representing the procedures
    * and a map giving the `Type` of each `Tree` that has one.
    */
  def typeTree(tree: Tree): (Seq[ProcedureScope], Map[Tree, Type]) = {
    assert(tree.production == "S BOF defdefs EOF")
    val defdefs = tree.children(1)

    val topLevelProcedureScopes = collect(defdefs, "defdef").map{defdef => new ProcedureScope(defdef, None)}
    val topLevelSymbolTable: SymbolTable =
      topLevelProcedureScopes.map{procedure => (procedure.name -> Right(procedure))}.toMap
    topLevelProcedureScopes.foreach{procedureScope => procedureScope.createSymbolTable(topLevelSymbolTable)}

    val allProcedureScopes = topLevelProcedureScopes.flatMap(procedureScope => procedureScope.descendantScopes)

    val typeMap: Map[Tree, Type] = allProcedureScopes.flatMap(typeCheck).toMap

    (allProcedureScopes, typeMap)
  }

  /** Compiles a Lacs program to MIPS machine language. */
  def compile(input: String): MachineCode = {
    val (procedureScopes, typeMap) = scanAndParseAndType(input)
    val procedures = generateProcedures(procedureScopes, typeMap)
    compilerA6(procedures)
  }

  /** Compiles a Lacs program and the `GarbageCollector` to MIPS machine language. */
  def compileWithGarbageCollector(input: String): MachineCode = {
    MemoryManagement.heap = GarbageCollector
    compile(input)
  }
}
