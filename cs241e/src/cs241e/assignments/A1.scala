/* ## Assignment 1 */

package cs241e.assignments

import cs241e.assignments.Assembler._
import cs241e.mips._

object A1 {
  /* As part of Assignment 1, before implementing the methods in this file, first implement the first four methods
   * in Assembler.scala.
   */

  /** The `setMem` method in `State` loads one word into a specific memory location identified by
    * a given address. Implement this extended `setMem` method that takes a sequence of `words` and writes them
    * into successive memory locations in `inputState` starting with a given `startingAddress`. Return the
    * resulting state.
    */

  def setMem(words: Seq[Word], inputState: State = State(), startingAddress: Word = Word.zero): State = {
    var state = inputState
    var address: Word = startingAddress

    words.foreach
    {

      x=>
        state = state.setMem(address, x)
        val newAddressInt = decodeUnsigned(address.toSeq)
        address = Word(encodeUnsigned(newAddressInt + 4))
    }
    state
  }

  /** You can use this helper method to test the following programs that you will write. It loads the
    * given `words` into memory, writes the specified values to registers 1 and 2, and then runs
    * the CPU on the `words` that were loaded.
    */
  def loadAndRun(words: Seq[Word], register1: Word = Word.zero, register2: Word = Word.zero): State = {
    val initialState =
      setMem(words)
        .setReg(1, register1)
        .setReg(2, register2)
    CPU.run(initialState)
  }

  /** Write a MIPS machine language program that returns to the address saved in register 31. This is the only thing
    * that your program should do.
    *
    * Hint: You can create a `Word` of 32 bits as follows: `Word("01010101010101010101010101010101")`.
    */
  val w = JR(31)

  lazy val returnTo31 = Seq[Word](w)

  /** Write a MIPS machine language program that copies the value in register 1 to register 3, then adds the values
    * in register 1 and 3, placing the result in register 4, and then returns.
    */

  val w1 = ADD(3,1,0)
  val w13 = ADD(4,1,3)
  lazy val add134 = Seq[Word](w1,w13, w)

  /* Now implement the code generation methods in the second half of `Assembler.scala`. Then continue here
   * with the following methods.
   */

  /** Write a MIPS machine language program that determines the maximum of the values in registers 1 and 2
    * interpreted as two's-complement integers, places it in register 3, and returns.
    */
  val w2 = SLT(4, 1, 2)
  val w3 = ADD(3, 1, 0)
  val w4 = BEQ(4, 0, 1)
  val w5 = ADD(3, 2, 0)
  lazy val maximum = Seq[Word](w2, w3, w4, w5, w)

  /** Write a MIPS machine language program that adds 1 to the value in register 1, places the result in register 3,
    * and then returns.
    */
  val w6 = LIS(4)
  val w7 = Word("00000000000000000000000000000001")
  val w8 = ADD(3, 1, 4)

  lazy val addOne = Seq[Word](w6, w7, w8, w)

  /** Write a MIPS machine language program that interprets the value in register 1 as the address of a word
    * in memory, places the address of the following word in memory in register 3, and then returns.
     */
  val w9 = LIS(5)
  val w10 = Word("00000000000000000000000000000100")
  val w11 = ADD(3, 1, 5)
  lazy val followingAddress = Seq[Word](w9, w10, w11, w)
}

