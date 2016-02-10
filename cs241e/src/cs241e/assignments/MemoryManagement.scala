package cs241e.assignments

import cs241e.assignments.Assembler._
import cs241e.assignments.CodeBuilders._
import cs241e.assignments.ProgramRepresentation._
import cs241e.mips._

/** An implementation of a strategy to lay out and keep track of memory, both on the stack and on the heap. */

object MemoryManagement {

  /** To make it easier to keep track of which areas of memory are used to store which data, we
    * organize all memory in `Chunk`s. A `Chunk` is a compile-time representation of multiple
    * words of memory indexed by a sequence of `Variable`s. Each variable is assigned a fixed
    * offset from the beginning of the `Chunk`. The `Chunk` can generate the
    * code to read and write each variable. A `Chunk` is associated with a `baseRegister`.
    * The code that is generated to read/write a variable assumes that the `baseRegister`
    * holds the address of the beginning of the `Chunk`.
    *
    * When a `Chunk` is represented in memory at run time, word 0 of the `Chunk`
    * always holds the size of the `Chunk` in bytes. This makes it possible to deallocate
    * or copy the run-time instance of the `Chunk` in memory without knowing the specific
    * static `Chunk` that it represents. Word 1 of the `Chunk` is reserved for Assignment 11
    * (discussed in the next paragraph). To summarize, the memory layout looks as follows:
    * word 0: size in bytes (4 * (2 + n), where n = number of variables)
    * word 1: reserved for Assignment 11
    * words 2 to n+1: variables
    *
    * Starting in Assignment 11, it is also necessary to know at run time which `Variable`s
    * in the run-time `Chunk` are pointers to other `Chunk`s (have their isPointer == true).
    * To enable this, word 1 of the
    * `Chunk` always holds the number of pointer `Variable`s in the chunk. In addition,
    * all of the pointer `Variable`s are allocated first, starting from word 2
    * of the `Chunk`, followed by all of the non-pointer variables. To summarize, the
    * full memory layout looks as follows:
    * word 0: size in bytes (4 * (2 + n), where n = number of variables)
    * word 1: p = number of pointer variables
    * words 2   to 2+p-1: pointer variables
    * words 2+p to 2+n-1: non-pointer variables
    */
  case class Chunk(variables: Seq[Variable], baseRegister: Int = Reg.framePointer) {
    /** For Assignment 11, the variables need to be sorted as described above, so we sort them here.
      * For earlier assignments, the order of the variables is irrelevant so sorting them here doesn't affect anything.
      */
    private val sortedVariables: Seq[Variable] = {
      val (pointers, nonPointers) = variables.partition(_.isPointer)
      pointers ++ nonPointers
    }

    /** The amount of memory needed to store the chunk in words. */
    val words: Int = 2 + variables.size
    /** The amount of memory needed to store the chunk in bytes. */
    val bytes: Int = words * 4

    /** Returns the same `Chunk` but with a different `baseRegister`. */
    def withBaseRegister(newBaseRegister: Int) = copy(baseRegister = newBaseRegister)

    /** Maps each variable to the offset of its address from the start of the `Chunk`.
      *
      * Scala hint:
      *
      * Seq('a', 'b', 'c')
      *   .zipWithIndex.map{case (letter, index) => (letter, index*2)}
      *   .toMap ==
      * Map('a' -> 0, 'b' -> 2, 'c' -> 4)
      *
      */
    private val variableToOffset: Map[Variable, Int] = {
      variables.zipWithIndex.map{case (variable, index) => (variable, index * 4 + 8)}.toMap
    }

    /** Generate code to read the value of `variable` from the `Chunk` in memory into `register`. */
    def read(register: Int, variable: Variable): Code = access(variable, register, read = true)
    /** Generate code to write the value of `register` into `variable` in the `Chunk` in memory. */
    def write(variable: Variable, register: Int): Code = access(variable, register, read = false)
    /** Generate code to read/write the value of `variable` into/out of `register`, depending on
      * whether `read` is true/false.
      */
    def access(variable: Variable, register: Int, read: Boolean): Code = {
      val offset = variableToOffset(variable)
      val w1 = LW(register,offset,baseRegister)
      if(read) {
        CodeWord(Word(w1))
      } else {
        val w2 = SW(register,offset,baseRegister)
        CodeWord(Word(w2))
      }
    }

    /** Generate code to initialize a `Chunk` that has just been allocated. The generated code should
      * assume that register `Reg.allocated` contains the address of the beginning of the `Chunk`.
      * It should write the size of the `Chunk` in bytes into word 0 of the `Chunk`.
      * It should set the values of all of the variables in the `Chunk` to 0.
      * Starting in Assignment 11, it should also write the number of pointer variables into word 1
      * of the `Chunk`.
      */
    def initialize: Code = {
      val w1 = CodeWord(LIS(4))
      val w2 = CodeWord(Word(encodeUnsigned(bytes)))
      val w3 = CodeWord(SW(4,0,Reg.allocated))
      var seqOfCode :Seq[Code] = Seq( Comment("Initializing chunk..."),w1,w2,w3)
      variables.foreach {
        x=>
          val offset = variableToOffset(x)
          val setToZero: Code = CodeWord(SW(0,offset,Reg.allocated))
          seqOfCode = seqOfCode :+ setToZero
      }
      Block(seqOfCode)
    }
  }

  /** An abstract memory allocator that allocates memory either on the stack or on the heap. */
  abstract class MemoryAllocator {
    /** The code to initialize the memory allocator at the beginning of the program. By default,
      * no initialization is necessary.
      */
    val initCode: Code = block()

    /** Generate the code to allocate enough space to hold `chunk` and place the address of the allocated `Chunk`
      * in `Reg.allocated`. This code should be followed by the code generated by `chunk.initialize`.
      */
    def allocate(chunk: Chunk): Code
  }

  /** A `MemoryAllocator` that allocates `Chunk`s of memory on the stack. */
  object Stack extends MemoryAllocator {
    /** Generate the code to allocate enough space to hold `chunk` and place the address of the allocated `Chunk`
      * in `Reg.allocated`. This code should be followed by the code generated by `chunk.initialize`.
      */
    def allocate(chunk: Chunk): Code = block(
      CodeWord(LIS(4)),
      CodeWord(Word(encodeUnsigned(chunk.bytes))),               // load the size of te chuck and place it in register 4
      CodeWord(SUB(Reg.stackPointer, Reg.stackPointer, 4)),      //
      //CodeWord(ADD(chunk.baseRegister, 0 ,Reg.stackPointer)),
      CodeWord(ADD(Reg.allocated, 0, Reg.stackPointer)),
      chunk.initialize
    )
    /** Deallocates the space for the `Chunk` that is at the top of the stack. To determine the size of this
      * `Chunk`, takes advantage of the convention that word 0 of each `Chunk` stores its size in bytes.
      */
    val pop: Code = block(
      CodeWord(LW(4, 0, Reg.stackPointer )), // reads the size of the chunk into register 4
      CodeWord(ADD(Reg.stackPointer, Reg.stackPointer, 4))
      //CodeWord(ADD(Reg.allocated, Reg.allocated, 4))
    )
  }

  /** Code that copies a chunk whose address is in `fromRegister` to the address in `toRegister`.
    * `toRegister` and `fromRegister` cannot be one of the registers in `usedRegisters`.
    * Be careful to modify only the registers in `usedRegisters` in the copying code that
    * you generate.
    *
    * Also, do not use any Variables inside copyChunk, or any Code that depends on
    * them, that is, any code appearing after Block in ProgramRepresentation.scala,
    * particularly including whileLoop. This is because copyChunk will be used to
    * implement calls from one procedure to another, and it is not clear in which
    * procedure's frame such Variables are allocated.
    */
  def copyChunk(toRegister: Int, fromRegister: Int): Code = {
    /* The registers that may be modified by the code that will be generated. */
    val modifiedRegisters = Set(Reg.result, Reg.scratch, Reg.copyChunkScratch)
    require(!modifiedRegisters.contains(toRegister))
    require(!modifiedRegisters.contains(fromRegister))
    /*
        val w1 = CodeWord(LW(4,0,fromRegister))
        val w3 = CodeWord(ADD(3,0,0))             // set reg 3 to 0
        val w4 = CodeWord(ADD(4,fromRegister,0))
        val w5 = CodeWord(ADD(7,toRegister,0))
        val w6 = CodeWord(BEQ(3,fromRegister,???)) // starting of the loop
        val w7 = CodeWord(LW(4,0,fromRegister))   // load the memory of the address in the fromRegister, stored in register 3
        val w2 = CodeWord(SW(4,0,toRegister))     // store that memory  in the address in the toRegister

    */ ???
  }

  var heap: MemoryAllocator = SimpleHeapAllocator

  trait HeapSettings {
    /** The total number of bytes of memory. */
    val memSize = decodeUnsigned(CPU.maxAddr)
    /** The address of the beginning of the heap. */
    val heapStart = Word(encodeUnsigned(memSize / 4))
    /** The address of the middle of the heap. */
    val heapMiddle = Word(encodeUnsigned(memSize / 2))
    /** The address just after the end of the heap. */
    val heapTop = Word(encodeUnsigned(memSize * 3 / 4))
  }

  /** A simple `MemoryAllocator` that allocates `Chunk`s of memory on the heap in a way that they
    * are never freed. Specifically, `Reg.heapPointer` is assumed to point to the next unused
    * memory address in the heap. To allocate a `Chunk` of a given size, the allocator just returns
    * the current value of `Reg.heapPointer` and increments it by the size so that it points to the
    * next unused word.
    */
  object SimpleHeapAllocator extends MemoryAllocator with HeapSettings {
    /** The code to initialize the heap allocator. */
    override val initCode: Code = block(LIS(Reg.heapPointer), heapStart)

    /** Generate the code to allocate enough space to hold `chunk` and place the address of the allocated `Chunk`
      * in `Reg.allocated`. This code should be followed by the code generated by `chunk.initialize`.
      *
      * Note that `allocate` is called after `VarAccess` `Code`s have been eliminated. Therefore, the `Code` that
      * it returns must not contain any `VarAccess` `Code`s or `Code`s that are defined after `VarAccess` in
      * `ProgramRepresentation.scala`.
      */
    def allocate(chunk: Chunk): Code = block(
      CodeWord(ADD(Reg.allocated,0, Reg.heapPointer)),
      CodeWord(LIS(4)),
      CodeWord(Word(encodeUnsigned(chunk.bytes))),               // load the size of te chuck and place it in register 3
      CodeWord(ADD(Reg.heapPointer, Reg.heapPointer, 4)),
      chunk.initialize
    )
  }

  /* ## Assignment 11 */

  /** A `MemoryAllocator` that uses a copying garbage collector to reclaim `Chunk`s that are unreachable
    * from the current `Chunk` whose address is in `Reg.framePtr`. The heap is split into two halves (semispaces).
    * Memory is allocated from one of the semispaces. When the semispace becomes full, the garbage collector
    * is launched. The garbage collector copies all reachable `Chunk`s into the other semispace
    * and adjusts all pointer `Variable`s in all reachable `Chunk`s to point to the new copies. The other semispace
    * then becomes the current semispace from which memory is allocated until it becomes full, and then the whole
    * process is repeated.
    *
    * The provided `initCode` follows the assumption that `Reg.heapPointer` points to the next unused
    * word in the current semispace, and that `Reg.semiSpaceTop` points to the word immediately
    * after the end of the current semispace.
    *
    * The first semispace starts at address heapStart and ends just before heapMiddle, and
    * the second semispace starts at address heapMiddle and ends just before heapTop.
    */

  object GarbageCollector extends MemoryAllocator with HeapSettings {
    /** The code to initialize the heap allocator. */
    override val initCode: Code = block(LIS(Reg.heapPointer), heapStart, LIS(Reg.semiSpaceTop), heapMiddle)

    /** Generate the code to allocate enough space to hold `chunk` and place the address of the allocated `Chunk`
      * in `Reg.allocated`. This code should be followed by the code generated by `chunk.initialize`.
      *
      * Note that `allocate` is called after `VarAccess` `Code`s have been eliminated. Therefore, the `Code` that
      * it returns must not contain any `VarAccess` `Code`s or `Code`s that are defined after `VarAccess` in
      * `ProgramRepresentation.scala`.
      *
      * For this reason, most of the code of the garbage collector can be implemented in `Procedure`s in the
      * value `procedures` below. These procedures may use all `Code`s, since they will be compiled with the
      * full compiler. The `allocate` method should return only the code needed to call the relevant `Procedure`.
      * However, since it cannot contain the `Call` `Code`, the code to call the procedure must be implemented
      * directly in terms of simpler `Code`s.
      *
      * If there is not enough space in the current semispace to allocate the `chunk`, `allocate` should call the
      * garbage collector to try to free up space. If there is still not enough space after the garbage collection
      * pass, the behaviour of `allocate` is undefined.
      */
    def allocate(chunk: Chunk): Code = {
      block(
        //         ???,
        //         chunk.initialize
      )
    }



    /** The procedure that performs a garbage collection pass over the heap. The procedure should take
      * zero parameters, and it should return an `Int`, the number of free bytes remaining in the heap
      * after the garbage collection pass has been completed. If this procedure returns `n`, then a subsequent
      * allocation of an `n` byte `Chunk` should succeed, but there would not be enough memory left to
      * allocate a `Chunk` of `n+1` bytes.
      */
    //def collectGarbage: Procedure = ???

    /** The sequence of all auxiliary procedures required by the code that is returned by `allocate` above.
      * This sequence should include the `collectGarbage` procedure above.
      */
    def procedures: Seq[Procedure] = Seq()
  }
}
