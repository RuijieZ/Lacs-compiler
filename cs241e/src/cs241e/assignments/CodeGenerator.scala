package cs241e.assignments

import cs241e._

import ProgramRepresentation._

import CodeBuilders._
import Typer._
import scanparse.Grammars._
import Assembler._
import cs241e.mips._

/** A code generator that converts a Lacs parse tree into the intermediate language developed in Assignment 1 to 6. */
object CodeGenerator {
  def generateProcedures(procedureScopes: Seq[ProcedureScope], typeMap: Map[Tree, Type]): Seq[Procedure] = {

    /** Given a `procedureScope`, generates the `Code` implementing the procedure, and assigns it to
      * `procedureScope.procedure.code`.
      */
    def generateCode(procedureScope: ProcedureScope): Unit = {

      /** Generates the `Code` that implements `tree`.
        *
        * This method will be called from the outside only on `tree`s rooted at nodes of kind "expras".
        * However, if it calls itself recursively on other kinds of nodes, it needs to be able to handle
        * those node kinds as well.
        */
      def recur(tree: Tree): Code = {
        val l = new Label("whats up")
        if(tree.children.length == 1 && tree.lhs.kind == "expras") { //"expras" => expr
          recur(tree.children(0))
        } else if (tree.children.length == 3 && tree.lhs.kind == "expras") {
          block(recur(tree.children(0)),recur(tree.children(2)))
        } else if (tree.children.length == 1 && tree.lhs.kind == "expra") {
          recur(tree.children(0))
        } else if (tree.children.length == 3 && tree.lhs.kind == "expra") {
          val v = procedureScope.symbolTable(tree.children(0).lhs.lexeme) match {
            case Left(x)  => x.variable
          }
          block(recur(tree.children(2)), write(v,3))
        } else if (tree.children.length == 11) {
          ifStmt(recur(tree.children(2).children(0)), tree.children(2).children(1).lhs.kind match {
            case "NE" => neCmp
            case "LT" => ltCmp
            case "LE" => leCmp
            case "GE" => geCmp
            case "GT" => gtCmp
            case "EQ" => eqCmp
          }, recur(tree.children(2).children(2)),
           recur(tree.children(5)), recur(tree.children(9)))
        } else if (tree.children.length == 1 && tree.lhs.kind == "expr") {
          recur(tree.children(0))
        } else if (tree.children.length == 3 && tree.lhs.kind == "expr") {
          val code: Code = tree.children(1).lhs.kind match {
            case "PLUS" => plus
            case "MINUS" => minus
          }
          binOp(recur(tree.children(0)), code, recur(tree.children(2)))
        } else if (tree.children.length == 1 && tree.lhs.kind == "term") {
          recur(tree.children(0))
        } else if (tree.children.length == 3 && tree.lhs.kind == "term") {
          val code: Code = tree.children(1).lhs.kind match {
            case "STAR" => times
            case "SLASH" => divide
            case "PCT" => remainder
          }
          binOp(recur(tree.children(0)), code, recur(tree.children(2)))
        } else if (tree.children.length == 1 && tree.lhs.kind == "factor") {
          tree.children(0).lhs.kind match {
            case "ID" => procedureScope.symbolTable(tree.children(0).lhs.lexeme) match {
                              case Left(x)  => read(3,x.variable)
                              case Right(s) => Closure(s.procedure)
                         }
            case "NUM" => block(CodeWord(LIS(3)),Word(encodeSigned(tree.children(0).lhs.lexeme.toInt)))

          }
        } else if (tree.children.length == 3 && tree.lhs.kind == "factor") {
          recur(tree.children(1))
        } else { // must a factor with length 4
          tree.children.head.production match  {
            case "factor ID" => {
              procedureScope.symbolTable(tree.children.head.children.head.lhs.lexeme) match {
                case Left(x) => CallClosure(
                  read(3,x.variable),
                  collect(tree.children(2), "expr").map(x=>recur(x)),
                  collect(tree.children(2), "expr").map(x=>new Variable(""))
                )
                case Right(scope) => Call(scope.procedure, collect(tree.children(2), "expr").map(x=>recur(x)))
              }
            }
            case _ => {
              CallClosure (
                recur(tree.children.head),
                collect(tree.children(2), "expr").map(x=>recur(x)),
                collect(tree.children(2), "expr").map(x=>new Variable("this assignment is too long"))
              )
            }
          }
        }

      }

      /* Main body of generateCode. */
      procedureScope.procedure.code = recur(procedureScope.expras)
    }

    /* Main body of generateProcedures. */

    procedureScopes.foreach(generateCode)
    procedureScopes.map(_.procedure)
  }
}


