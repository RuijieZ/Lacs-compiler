package cs241e.assignments

import cs241e.mips._
import Assembler._
import Transformations._
import ProgramRepresentation._
import CodeBuilders._
import A1._

object A4 {
  /** A utility method that takes two sequences of words representing `code` and an `array`,
    * loads both into memory, and sets register 1 to the address of the beginning
    * of the array and register 2 to its length in words.
    */
  def loadCodeAndArray(code: Seq[Word], array: Seq[Word]): State = {
    val arrayAddress: Word = Word(encodeUnsigned(code.size * 4))
    val arrayLength: Word = Word(encodeUnsigned(array.size))
    val loadedCode: State = setMem(code)
    val loadedArray: State = setMem(array, loadedCode, arrayAddress)
    loadedArray.setReg(1, arrayAddress).setReg(2, arrayLength)
  }

  /** A utility method that loads code and an array into memory and runs the code. */
  def loadAndRunArray(code: MachineCode, array: Seq[Word], debug: Boolean = false): State = {
    val state = loadCodeAndArray(code.words, array)
    if(debug) Debugger.debug(state, code.debugTable)
    else CPU.run(state)
  }

  /** Register 1 holds the address of the beginning of an array of 32-bit integers.
    * Register 2 holds the number of elements in the array.
    * If the array is empty place the value -1 in register 3.
    * Otherwise copy the last element of the array into register 3, and return.
    */
  lazy val lastElement: MachineCode = {
    val label1 = new Label("label1")
    val code: Code = block(
      LIS(10),
      Word("00000000000000000000000000000001"),
      BNE(2,0,3),
      ADD(3,0,0),
      SUB(3,0,10),
      beq(1,1,label1),  // Copy the -1 into $3 if 2 equals to 0
      SUB(11,2,10),
      LIS(10), Word("00000000000000000000000000000100"),
      MULT(11,10), MFLO(11), ADD(11, 1, 11), LW(3,0,11),Define(label1), JR(31)

    )
    compilerA4(code, Seq())
  }

  /** Register 1 holds the address of the beginning of an array of 32-bit two's-complement integers.
    * Register 2 holds the number of elements in the array.
    * Determine the maximum of all the elements of the array, write it into register 3, and return.
    * Assume the array is not empty.
    */

  lazy val arrayMaximum: MachineCode = {
    val load4 = block(LIS(3), Word("00000000000000000000000000000100"))// loads value of 4 in $3
    val i = new Variable("index")
    val candidate = new Variable("candidate")
    val memOfA0 = block (
      LW(16,0,1),
      ADD(3,16,0)
    )

    val readI = read(4,i)
    val variables = Seq[Variable](candidate,i)
    val e1 = read(3,i)
    val e2 = ADD(3,0,2)
    val e3 = block(load4, readI, times, ADD(3,3,1),LW(3,0,3))                                        // get the value of A[i] stores in reg3
    val e4 = read(3,candidate)                                                             // get the value of candidate in reg3
    val thens = block(load4, readI, times, ADD(3,3,1),LW(4,0,3), write(candidate,4))
    val incrementI = block(LIS(10), Word("00000000000000000000000000000001"),read(20,i),ADD(20,20,10),write(i,20))

    val body = block(ifStmt(e3,gtCmp,e4,thens), incrementI)
    val code: Code = block (
      write(i,0), // set i=0
      memOfA0,
      write(candidate,3),                                                                                               // set candidate = A[i]
      whileLoop(e1,ltCmp,e2,body),
      read(3,candidate),
      JR(31)
    )
    compilerA4(code, variables)
  }

  /** Register 1 holds the address of the beginning of an array of 32-bit integers, each representing a character.
    * The integer zero represents a space, and each integer i (1 <= i <= 26) represents the i'th letter of the
    * uppercase alphabet.
    * Register 2 holds the number of elements in the array (can be empty).
    * Your program should output the uppercase characters represented by the integers in the array, and return.
    * The MIPS system allows you to output one character at a time, by storing its ASCII value into the
    * special memory location ffff000c (hex).
    *
    * Hint: use Google to find an ASCII code table.
    */
  
  lazy val outputLetters: MachineCode = {
    val label1 = new Label("label1")
    //val label2 = new Label("label2")
    val i = new Variable("i")
    val k = new Variable("k")
    val accessMem = block(
      LIS(14),
      Word("00000000000000000000000000000100"),
      read(15,i),
      MULT(14,15),
      MFLO(15),
      ADD(3,1,15)
    )
    val incrementI = block(read(20,i),ADD(20,20,10),write(i,20))
    val body = block(ifStmt(ADD(3,0,0),eqCmp,deref(accessMem),SW(11,0,13),block(deref(accessMem),ADD(16,3,12),SW(16,0,13))),incrementI)
    val variables = Seq[Variable](k,i)
    val code: Code = block(
      LIS(11),
      Word("00000000000000000000000000100000"),
      LIS(12),
      Word("00000000000000000000000001000000"),
      LIS(10),
      Word("00000000000000000000000000000001"),
      LIS(13),
      Word("11111111111111110000000000001100"),
      LIS(21),
      Word("00000000000000000000000000011010"),
      BNE(2,0,1),
      beq(0,0,label1),
      write(i,0),
      whileLoop(read(3,i),ltCmp,ADD(3,2,0),body),
      Define(label1),
      JR(31)

    )
    compilerA4(code, variables)
  }

  /** Register 1 holds a 32-bit integer (in two's-complement notation).
    * Your program should format this integer in base 10, print it, then print a newline character, and return.
    */
  // 25 STORE THE SHANG








  val printLabel = new Label("print")
  lazy val printIntegerVariables: Seq[Variable] = Seq()
  lazy val printIntegerCode: Code = {

    def printMin: Code = block(
      SW(12,0,13),          // -
      LIS(18),
      Word(encodeUnsigned(50)),  // 2
      SW(18,0,13),
      LIS(18),
      Word(encodeUnsigned(49)),   //1
      SW(18,0,13),
      LIS(18),
      Word(encodeUnsigned(52)),   //4
      SW(18,0,13),
      LIS(18),
      Word(encodeUnsigned(55)),  //7
      SW(18,0,13),
      LIS(18),
      Word(encodeUnsigned(52)),  // 4
      SW(18,0,13),
      LIS(18),
      Word(encodeUnsigned(56)),  // 8
      SW(18,0,13),
      LIS(18),
      Word(encodeUnsigned(51)),  // 3
      SW(18,0,13),
      LIS(18),
      Word(encodeUnsigned(54)),  // 6
      SW(18,0,13),
      LIS(18),
      Word(encodeUnsigned(52)),  // 4
      SW(18,0,13),
      LIS(18),
      Word(encodeUnsigned(56)),  // 8
      SW(18,0,13)
    )

    def getFactorSaveTo20 = block(
      whileLoop(
        block(DIV(1,20),MFLO(3)),
        eqCmp,
        ADD(3,0,0),
        block (DIV(20,10),MFLO(20))
      )
    )
    def printPositive: Code = block (
      getFactorSaveTo20,
      whileLoop(
        ADD(3,20,0),
        neCmp,
        ADD(3,0,0),
        block (
          DIV(1,20),
          MFLO(25),
          ADD(25,25,16),
          SW(25,0,13),
          DIV(1,20),
          MFHI(1),
          DIV(20,10),
          MFLO(20)
        )
      )
    )

    def printNegative = ifStmt(
      ADD(3,0,1),
      eqCmp,
      ADD(3,0,17),
      printMin,
      block(
        SUB(1,0,1),
        SW(12,0,13),
        printPositive

      )



    )

    val label1 = new Label("label1_1")
    val label4 = new Label("label2_1")
    val label3 = new Label("label3_1")
    block(

      LIS(13),
      Word("11111111111111110000000000001100"),
      LIS(12),
      Word("00000000000000000000000000101101"),
      LIS(20),
      Word(encodeUnsigned(1000000000)),
      LIS(15),                                              // 15 stores 1
      Word("00000000000000000000000000000001"),
      LIS(16),
      Word(encodeUnsigned(48)),
      LIS(17),
      Word(encodeSigned(-2147483648)),
      LIS(10),
      Word("00000000000000000000000000001010"),
      bne(1,0,label3),
      LIS(1),
      Word(encodeUnsigned(48)),
      SW(1,0,13),
      beq(0,0,label4),
      Define(label3),
      SLT(24, 1, 0),                                          // 24 stores whether it is positive or negative
      beq(24,15,label1),
      printPositive,
      beq(0,0,label4),
      Define(label1),
      printNegative,
      Define(label4),
      SW(10,0,13)
      //JR(31)
    )


  }

  //***************************************************************************************
  def printIntergerCodeFunction(): Code = {
    //val printLabel = new Label("print")
    lazy val printIntegerVariables: Seq[Variable] = Seq()
    lazy val printIntegerCode2: Code = {

      def printMin: Code = block(
        SW(12,0,13),          // -
        LIS(18),
        Word(encodeUnsigned(50)),  // 2
        SW(18,0,13),
        LIS(18),
        Word(encodeUnsigned(49)),   //1
        SW(18,0,13),
        LIS(18),
        Word(encodeUnsigned(52)),   //4
        SW(18,0,13),
        LIS(18),
        Word(encodeUnsigned(55)),  //7
        SW(18,0,13),
        LIS(18),
        Word(encodeUnsigned(52)),  // 4
        SW(18,0,13),
        LIS(18),
        Word(encodeUnsigned(56)),  // 8
        SW(18,0,13),
        LIS(18),
        Word(encodeUnsigned(51)),  // 3
        SW(18,0,13),
        LIS(18),
        Word(encodeUnsigned(54)),  // 6
        SW(18,0,13),
        LIS(18),
        Word(encodeUnsigned(52)),  // 4
        SW(18,0,13),
        LIS(18),
        Word(encodeUnsigned(56)),  // 8
        SW(18,0,13)
      )

      def getFactorSaveTo20 = block(
        whileLoop(
          block(DIV(1,20),MFLO(3)),
          eqCmp,
          ADD(3,0,0),
          block (DIV(20,10),MFLO(20))
        )
      )
      def printPositive: Code = block (
        getFactorSaveTo20,
        whileLoop(
          ADD(3,20,0),
          neCmp,
          ADD(3,0,0),
          block (
            DIV(1,20),
            MFLO(25),
            ADD(25,25,16),
            SW(25,0,13),
            DIV(1,20),
            MFHI(1),
            DIV(20,10),
            MFLO(20)
          )
        )
      )

      def printNegative = ifStmt(
        ADD(3,0,1),
        eqCmp,
        ADD(3,0,17),
        printMin,
        block(
          SUB(1,0,1),
          SW(12,0,13),
          printPositive

        )



      )

      val label1 = new Label("label1_1")
      val label4 = new Label("label2_1")
      val label3 = new Label("label3_1")
      block(

        LIS(13),
        Word("11111111111111110000000000001100"),
        LIS(12),
        Word("00000000000000000000000000101101"),
        LIS(20),
        Word(encodeUnsigned(1000000000)),
        LIS(15),                                              // 15 stores 1
        Word("00000000000000000000000000000001"),
        LIS(16),
        Word(encodeUnsigned(48)),
        LIS(17),
        Word(encodeSigned(-2147483648)),
        LIS(10),
        Word("00000000000000000000000000001010"),
        bne(1,0,label3),
        LIS(1),
        Word(encodeUnsigned(48)),
        SW(1,0,13),
        beq(0,0,label4),
        Define(label3),
        SLT(24, 1, 0),                                          // 24 stores whether it is positive or negative
        beq(24,15,label1),
        printPositive,
        beq(0,0,label4),
        Define(label1),
        printNegative,
        Define(label4),
        SW(10,0,13)
        //JR(31)
      )


    }
    printIntegerCode2
  }
  //***************************************************************************************






  lazy val printInteger: MachineCode = compilerA4(printIntegerCode, printIntegerVariables)
}



