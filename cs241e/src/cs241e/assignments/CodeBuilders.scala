package cs241e.assignments

import ProgramRepresentation._
import Assembler._
import cs241e.mips._

/** Methods that generate `Code` for various higher-level language features. */

object CodeBuilders { 
  /* ## Assignment 4 */

  /* Complete the implementation of the following methods by replacing the `???`. */

  /** Generates a binary operation that evaluates `e1` and `e2`, ensures that the `Reg.result` from
    * `e1` is in `Reg.scratch` and that the `Reg.result` from `e2` is still in `Reg.result`,
    * then executes `op`.
    *
    * Hint: Use withTempVar.
    */
  def binOp(e1: Code, op: Code, e2: Code): Code = {
      withTempVar(x => block(e1, write(x,Reg.result), e2, read(Reg.scratch, x), op))
  }

  /* The following `Code`s are intended to be used as the `op` argument to `binOp`.
   * They should expect two operands in `Reg.scratch` and `Reg.result`, compute the corresponding arithmetic
   * operation, and leave the result of the operation in `Reg.result`.
   *
   * Assume that the operands are to be interpreted as signed two's-complement integers except in
   * `divideUnsigned` and `remainderUnsigned`.
   */
  lazy val plus: Code = ADD(Reg.result, Reg.result, Reg.scratch)
  lazy val minus: Code = SUB(Reg.result, Reg.scratch, Reg.result)
  lazy val times: Code = block(MULT(4,3),MFLO(3))
  lazy val divide: Code = block(DIV(4,3), MFLO(3))
  lazy val remainder: Code = block(DIV(4,3),MFHI(3))
  lazy val divideUnsigned: Code = block(DIVU(4,3), MFLO(3))
  lazy val remainderUnsigned: Code = block(DIVU(4,3), MFHI(3))

  /* The following `Code`s are intended to be used as the `comp` argument to `IfStmt`.
   * They should expect two operands in `Reg.scratch` and `Reg.result`, interpret them as two's-complement
   * signed integers (except `gtUnsignedCmp`), compare them, and branch to `label` if the comparison fails.
   */
  def eqCmp(label: Label): Code = bne(3,4,label)
  def neCmp(label: Label): Code = beq(3,4,label)
  def ltCmp(label: Label): Code = block(SLT(19,4,3),beq(19,0,label))
  def gtCmp(label: Label): Code = block(SLT(19,3,4),beq(19,0,label))
  def leCmp(label: Label): Code = block(SLT(19,3,4),bne(19,0,label))
  def geCmp(label: Label): Code = block(SLT(19,4,3),bne(19,0,label))
  def gtUnsignedCmp(label: Label): Code = block(SLTU(19,3,4),beq(19,0,label))

  /** Generates code that evaluates `expr` to yield a memory address, then loads the word from that address
    * into `Reg.result`. */
  def deref(expr: Code) = block(expr, LW(3,0,3))

  /** Generates code that evaluates `target` to yield a memory address, then evaluates `expr` to yield a value,
    * then stores the value into the memory address.
    */
  def assignToAddr(target: Code, expr: Code) = block(target,ADD(10,3,0),expr,SW(3,0,10))

  /** Generates code that implements a while loop. The generated code should evaluate `e1` and `e2`,
    * compare them using `comp`, and if the comparison succeeds, it should execute `body` and repeat
    * the entire process from the beginning.
    */
  def whileLoop(e1: Code, comp: Label=>Code, e2: Code, body: Code): Code = {
    val label100 = new Label("label100")
    val label200 = new Label("label200")
    block (
      Define(label200),
      withTempVar(x => block(e1, write(x,Reg.result), e2, read(Reg.scratch, x))),
      comp(label100),
      body,
      beq(3,3,label200),
      Define(label100)
    )
  }

  
}