package cs241e.assignments

/** Here, we give symbolic names to registers to keep track of what we will use each register for. Feel free
  * to add your own register names here for your own uses of registers.
  */
object Reg {
  val savedPC = 31
  val stackPointer = 30
  val framePointer = 29
  val heapPointer = 28
  val semiSpaceTop = 27

  val scratch2 = 10
  val input1 = 1
  val input2 = 2
  val result = 3
  val scratch = 4
  val savedParamPtr = 5
  val allocated = 6
  val copyChunkScratch = 7
  val targetPC = 8
  val scratchPtrForGC = 9
}
