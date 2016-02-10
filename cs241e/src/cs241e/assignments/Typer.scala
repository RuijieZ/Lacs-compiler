package cs241e.assignments

import ProgramRepresentation._
import Lacs._
import cs241e.scanparse.Grammars._

import scala.collection.mutable

/** Implementation of semantic analysis for the Lacs language. */

object Typer {
  /** Representation of a Lacs type, which is either an Int or a function type with parameter types and a return type.
    */
  sealed abstract class Type
  case object IntType extends Type
  case class FunctionType(parameterTypes: Seq[Type], returnType: Type) extends Type

  /** Given a `tree`, finds all descendants of the `tree` whose root node has kind `lhsKind`.
    * Does not search within the found subtrees for any nested occurrences of additional descendants.
    *
    * For example, searching the root of a program tree with `lhsKind = "procedure"` will return the trees all
    * of the top-level procedures, but not any procedures nested within them.
    */
  def collect(tree: Tree, lhsKind: String): Seq[Tree] =
    if(tree.lhs.kind == lhsKind) Seq(tree) else tree.children.flatMap((tree: Tree) => collect(tree, lhsKind))

  /** Given a tree that is either a "type" or contains exactly one "type" nested within it, returns
    * an instance of `Type` representing the corresponding type.
    */
  def parseType(tree: Tree): Type = {
    val types = collect(tree, "type")
    require(types.size == 1)    // why do we need to check if the size is 1

    if(types.head.children.size == 1) IntType
    //else if(types.head.children(1).children.isEmpty) FunctionType(Seq(),parseType(types.head.children(4)))
    else {
      val temp = collect(types.head.children(1), "type")
      val tempTree = temp.map(x => parseType(x))
      FunctionType(tempTree, parseType(types.head.children(4)))
    }
  }

  /** A variable combined with its declared type. */
  case class TypedVariable(variable: Variable, tpe: Type)

  /** Create a new `Variable` given its `name` and type `tpe`. */
  def makeVariable(name: String, tpe: Type): Variable =
    new Variable(name, isPointer = (tpe != IntType))

  /** A `SymbolTable` maps each name to either a `TypedVariable` or a `ProcedureScope`. */
  type SymbolTable = Map[String, Either[TypedVariable, ProcedureScope]]

  /** Given a tree containing subtrees rooted at "vardef", creates a `TypedVariable` for each such tree. */
  def parseVarDefs(tree: Tree): Seq[TypedVariable] = {
    collect(tree, "vardef").map{ varDef =>
      val t = varDef.children(2)
      val name = varDef.children(0).lhs.lexeme
      TypedVariable(makeVariable(name,parseType(t)),parseType(t))
    }
  }

  /** Call `error()` if any `String` occurs in `names` multiple times. */
  def checkDuplicates(names: Seq[String]): Unit = {
    val duplicates = names.diff(names.distinct)
    if(duplicates.nonEmpty) error(s"Duplicate identifiers ${duplicates}")
  }

  /** A `ProcedureScope` holds the semantic information about a particular procedure that is needed to type-check
    * the body of the procedure, including information coming from outer procedure(s) within which this
    * procedure may be nested.
    *
    * @param tree the tree defining the procedure (rooted at a "defdef")
    * @param outer the `ProcedureScope` of the outer procedure that immediately contains this one
    */
  class ProcedureScope(tree: Tree, outer: Option[ProcedureScope] = None) {
    assert(tree.production ==
      "defdef DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE")
    val Seq(_, id, _, parmsopt, _, _, retTypeTree, _, _, vardefs, defdefs, expras, _) = tree.children

    /** The name of the procedure. */
    val name: String = id.lhs.lexeme

    /** The parameters of the procedure. */
    val parms: Seq[TypedVariable] = parseVarDefs(parmsopt)

    /** The variables declared in the procedure. */
    val variables: Seq[TypedVariable] = parseVarDefs(vardefs)

    /** The declared return type of the procedure. */
    val returnType: Type = parseType(retTypeTree)

    /** The new `Procedure` object that will represent this procedure. */
    val procedure: Procedure = {
      if (outer == None) {
        new Procedure(name, parms.map(x => x.variable), variables.map(x => x.variable), None)
      } else {
        new Procedure(name, parms.map(x => x.variable), variables.map(x => x.variable), Some(outer.get.procedure))
      }
    }
    /** The `ProcedureScope`s of the nested procedures that are immediately nested within this procedure.
      *
      * Note: this `val` will recursively call `new ProcedureScope(...)`.
      */
    val subProcedures: Seq[ProcedureScope] = {
      val temp = collect(defdefs, "defdef")
      temp.map(x => new ProcedureScope(x, Some(this)))
    }

    /** The names of parameters, variables, and nested procedures that are newly defined within this procedure
      * (as opposed to being inherited from some outer procedure).
      */
    val newNames: Seq[String] =  parms.map(x => x.variable.name) ++ variables.map(x => x.variable.name) ++ subProcedures.map(x => x.name)
    checkDuplicates(newNames)

    /** The symbol table to be used when type-checking the body of this procedure. It should contain all
      * symbols (parameters, variables, nested procedures) defined in this procedure, as well as those
      * defined in outer procedures within which this one is nested. Symbols defined in this procedure
      * override (shadow) those of outer procedures.
      *
      * The symbol table is first initialized to null, and filled in later, after this `ProcedureScope`
      * has been constructed, by calling the `createSymbolTable` method. This is necessary
      * because computation of the `symbolTable` depends on the `symbolTable`s of any
      * outer procedures within which this procedure is nested. However, their `symbolTable`s contain
      * the `ProcedureScope`s for the procedures nested within them, which are not created until after
      * the enclosing procedure is created.
      */
    var symbolTable: SymbolTable = null

    /** Create a symbol table for the current procedure, and update the `symbolTable` field with it.
      * See the comments for `val symbolTable` for details on what the `symbolTable` should contain.
      *
      * The `outerSymbolTable` parameter contains the symbol table of the enclosing scope (either an
      * outer procedure within which the current procedure is nested, or, if the current procedure
      * is a top-level procedure, a symbol table containing the names of all of the top-level procedures).
      *
      * This method must also recursively call `createSymbolTable` on all of its `subProcedures` to
      * construct their symbol tables as well.
      */
    def createSymbolTable(outerSymbolTable: SymbolTable): Unit =  {
      symbolTable = outerSymbolTable
      val totalSeqOfVar = parms ++ variables
      totalSeqOfVar.map(x => symbolTable += (x.variable.name -> Left(x)))
      val temp = subProcedures
      temp.map(x => symbolTable += (x.name -> Right(x)))
      subProcedures.foreach(x=> x.createSymbolTable(symbolTable))
    }

    /** Returns a sequence containing `this` `ProcedureScope` and the `ProcedureScope`s for all procedures
      * declared inside of this procedure, including those nested recursively within other nested procedures.
      *
      * Scala hint: learn about the `flatMap` method in the Scala library. If you are not familiar with,
      * one place you can read about it is here:
      * http://www.artima.com/pins1ed/working-with-lists.html#sec:higher-order-methods
      */
    def descendantScopes: Seq[ProcedureScope] = Seq(this) ++ subProcedures.flatMap(x => x.descendantScopes)
  }

  /** Checks that the body of a procedure satisfies the type-checking rules in the Lacs language specification.
    * Returns a `Map` that provides a `Type` for each `Tree` that has a `Type` according to the language
    * specification.
    */

  def typeCheck(scope: ProcedureScope): Map[Tree, Type] = {
    /** The map that will be returned containing the `Type` of each `Tree` that has a `Type`. */
    val treeToType = mutable.Map[Tree, Type]()

    /** Calls `error()` if `tpe1` and `tpe2` are not equal. If they are equal, returns them. */
    def mustEqual(tpe1: Type, tpe2: Type): Type =
      if(tpe1 == tpe2) tpe1 else error(s"Type mismatch: expected $tpe2, got $tpe1")

    /** For a `tree` rooted at a node that has a `Type`, computes the `Type`, adds it to `treeToType`,
      * and returns it.
      */
    def typeOf(tree: Tree): Type = treeToType.getOrElseUpdate(tree, {
      if(scope.symbolTable == null) {
        scope.createSymbolTable(Map[String, Either[TypedVariable, ProcedureScope]](scope.name -> Right(scope)))
      }
      if (tree.lhs.kind == "ID") {
        scope.symbolTable(tree.lhs.lexeme).fold(left => left.tpe, right => FunctionType(right.parms.map(p => p.tpe), right.returnType))
      } else if (tree.lhs.kind == "factor") {
        if (tree.children(0).lhs.kind == "ID") {
          return typeOf(tree.children(0))
        } else if (tree.children(0).lhs.kind == "NUM") {
           return IntType
        } else if (tree.children.length == 3) {
           return typeOf(tree.children(1))
        } else {
          var args = collect(tree.children(2), "expr")
          var factor = typeOf(tree.children(0))
          factor match {
            case FunctionType(para, returnType) =>
              if (args.length != para.length) {
                error("the number of arguments does not equal to the number of parameters")
              }
              else {
                for (i <- args.indices) {
                  mustEqual(typeOf(args(i)), para(i))
                }
                return returnType
              }
            case _ => error("the type of the factor's fisrt children is not procedure!")
          }
        }
        } else if (tree.lhs.kind == "term") {
          if (tree.children.length == 1) return typeOf(tree.children(0))
          else {
            mustEqual(typeOf(tree.children(0)), IntType)
            mustEqual(typeOf(tree.children(2)), IntType)
            return IntType
          }
        } else if (tree.lhs.kind == "expr") {
          if (tree.children.length == 1) return typeOf(tree.children(0))
          else if (tree.children.length == 3) {
            mustEqual(typeOf(tree.children(0)), IntType)
            mustEqual(typeOf(tree.children(2)), IntType)
            return IntType
          } else {
            mustEqual(typeOf(tree.children(2).children.head), IntType)
            mustEqual(typeOf(tree.children(2).children(2)), IntType)
            mustEqual(typeOf(tree.children(5)), typeOf(tree.children(9)))
            return typeOf(tree.children(5))
          }
        }
       else if (tree.lhs.kind == "expra") {
        if (tree.children.length != 1) {
          mustEqual(typeOf(tree.children.head), typeOf(tree.children(2))); return typeOf(tree.children(2))
        }
        else return typeOf(tree.children(0))
      } else  {
        // must be a expras
        return typeOf(tree.children.last)
      }
    }


    )

    /* Check that the type of the expression returned from the procedure matches the declared type of the procedure. */
    mustEqual(scope.returnType, typeOf(scope.expras))

    Map() ++ treeToType
  }
}
//         scope.symbolTable(tree.lhs.lexeme) match {
//           case TypedVariable => return TypedVariable
//           case scope.symbolTalbe.r => return FunctionType()
//