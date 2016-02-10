package cs241e.assignments

import Assembler._
import CodeBuilders._
import ProgramRepresentation._
import Transformations._
import cs241e.mips._

object A2 {
  /* As part of Assignment 2, before implementing the methods in this file, first implement the methods in
   * Transformations.scala in the section for Assignment 2.
   */

  /* Registers 1 and 2 hold 32-bit integers (in two's-complement notation). Place the maximum of these integers
   * in register 3, and return.
   */
  val l1 = new Label("a")
  /*
  val w1 = CodeWord(SLT(4, 1, 2))
  val w3 = CodeWord(ADD(3, 1, 0))
  val w4 = CodeWord(BEQ(4, 0, 1))
  val w5 = CodeWord(ADD(3, 2, 0))
  val w =  CodeWord(JR(31))
  */

  val w1 = CodeWord(SLT(4, 1, 2))
  val w3 = CodeWord(ADD(3, 1, 0))
  val w4 = BeqBne(Seq(false,false,false,true,false,false, false,false,true,false,false, false,false,false,false,false), l1)
  val w7 = beq(4,0,l1)
  val w5 = CodeWord(ADD(3, 2, 0))
  val w9 = Define(l1)
  val w =  CodeWord(JR(31))

  lazy val maximum: Seq[Word] = {
    val code = Seq[Code](w1, w3, w7, w5,w9, w)
    eliminateLabels(code)
  }

  /* Registers 1 and 2 hold 32-bit integers (in unsigned integer notation). Place the maximum of these integers
   * in register 3, and return.
   */
  val w6 = CodeWord(SLTU(4, 1, 2))



  lazy val maximumUnsigned: Seq[Word] = {
    val code = Seq[Code](w6,w3,w7,w5,w9,w)
    eliminateLabels(code)
  }
}

