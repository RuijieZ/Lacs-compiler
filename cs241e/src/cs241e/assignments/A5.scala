package cs241e.assignments

import cs241e.assignments.ProgramRepresentation._
import Transformations._
import CodeBuilders._
//import com.sun.org.apache.xpath.internal.operations.Variable
import cs241e.mips._
import Assembler._

object A5 {
  /** The code of `printInteger` from Assignment 4 encapsulated as a `Procedure`. */

  lazy val printProcedure: Procedure = {
    val p1 = new Variable("p1")
    //val v2 = new Variable("v2")
    val procedure = new Procedure("printProcedure", Seq(p1), Seq())
    procedure.code = block(
      read(1, p1),
      printIntegerCode()
    )
    procedure
  }

  def printIntegerCode(): Code = {
    A4.printIntegerCode
  }

  /** This procedure will be executed with an array of 32-bit integers as in Assignment 4.
    * It should take two parameters: the first is the address of the beginning of the array
    * and the second is the number of elements in the array.
    * The procedure should call `printProcedure` for each integer in the array in turn,
    * to print all the integers, and return.
    *
    * Test this procedure by compiling it with `printProcedure` and running it on various arrays.
    */
  lazy val printArray: Procedure = {
    val p1 = new Variable("p1")
    val p2 = new Variable("p2")
    val procedure = new Procedure("printArray", Seq(p1, p2), Seq())
    procedure.code = block(
      ADD(14, 0, 0), //  14 contains 0
      read(26, p2),
      whileLoop(
        ADD(3, 0, 14),
        neCmp,
        ADD(3, 26, 0),
        block(
          read(1, p1),
          LW(1, 0, 1),
          Call(printProcedure, Seq(ADD(3, 0, 1))),
          LIS(16),
          Word(encodeUnsigned(4)),
          LIS(15),
          Word(encodeUnsigned(1)), //  15 contains 1
          read(3, p1),
          ADD(3, 3, 16), // $3 = $3 + 4
          write(p1, 3),
          ADD(14, 14, 15) // Increment 14 by 1
        )
      )
    )
    procedure
  }

  /** This procedure will be executed with an array of 32-bit integers as in Assignment 4.
    * It should take two parameters: the first is the address of the beginning of the array
    * and the second is the number of elements in the array.
    *
    * You may use multiple procedures if you wish. Generate them and return them in a `Seq`.
    * The tests will execute the first procedure in the sequence.
    *
    * The task is to determine the height of a binary tree and return it (in `Reg.result`).
    * Assume that every tree contains at least one node and hence has a height of at least one.
    * Each node of the tree is encoded in three consecutive elements (words) of the array:
    * a two's-complement integer stored at the node, the node's left child, and the node's right child.
    * Each child is specified as the array index of the first element of the child node.
    * The integer -1 indicates that a node does not have a left or right child. For example, the following tree:
    *
    * 77
    * /  \
    * 22    -8
    * /  \
    * -36   999
    *
    * could be encoded by following array:
    *
    * A[0] = 77
    * A[1] = 3
    * A[2] = 6
    * A[3] = 22
    * A[4] = -1
    * A[5] = -1
    * A[6] = -8
    * A[7] = 9
    * A[8] = 12
    * A[9] = -36
    * A[10] = -1
    * A[11] = -1
    * A[12] = 999
    * A[13] = -1
    * A[14] = -1
    *
    * in which the root is encoded by the elements A[0], A[1] and A[2], the root's left child is encoded
    * by the elements A[3], A[4] and A[5], the root's right child is encoded by the elements A[6], A[7] and A[8],
    * the root's left-most grandchild is encoded by the elements A[9], A[10] and A[11],
    * and the root's right-most grandchild is encoded by the elements A[12], A[13] and A[14].
    *
    * This example tree has height 3.
    */
  lazy val treeHeight: Seq[Procedure] = {

    val begin = new Variable("begin")
    val current = new Variable("r")


    val p1 = new Variable("p1")
    val p2 = new Variable("p2")
    val negOne = block(LIS(10), Word(encodeSigned(-1)))
    val posOne = block(LIS(16), Word(encodeSigned(1)))
    def readNum(i: Int): Code = {
      // take in an resigter number, read the value in it = k
      // find kth element of the array, store it in $12
      block(read(11, begin), LIS(12), Word(encodeUnsigned(4)), MULT(12, i), MFLO(12), ADD(11, 12, 11), LW(12, 0, 11))
    }

    val helper = new Procedure("helper", Seq(current, begin), Seq(p1))

    helper.code =

      ifStmt(
        block(read(3, current), ADD(13, 16, 3), ADD(14, 16, 13), readNum(13), ADD(3, 12, 0), readNum(14), ADD(3, 12, 3)),
        eqCmp,
        ADD(3, 10, 10), // $3 now contains the -2
        ADD(0, 0, 0), //  do nothing
        // else, add 1 and do the recursion call
        block(
          ADD(15, 15, 16),          // add 1
          ifStmt(
            block(read(3, current), ADD(13, 16, 3), readNum(13), ADD(3, 12, 0)), // LOAD THE CURRENT PLUS 1 POSTITION NUMBER
            eqCmp,
            ADD(3, 0, 10),    // check if the left tree is empty
            block(read(3, current),ADD(13, 3, 16),ADD(13,13,16),readNum(13), write(current, 12), Call(helper, Seq(read(3, current),read(3,begin)))), // call recursion on the right tree
              ifStmt(  //else, check if the right tree is empty
                block(read(3, current), ADD(13, 16, 3), ADD(13, 13, 16), readNum(13), ADD(3, 12, 0)), // LOAD THE CURRENT PLUS 2 POSITIION NUMBER
                eqCmp,
                ADD(3, 0, 10),
                block(read(3,current), ADD(13,16,3), readNum(13),write(current,12), Call(helper, Seq(read(3, current),read(3,begin)))), // call the recursion on the left tree
                // else, both trees are not empty, now we actually need to do two recursion call, and compare to get the max
                block(block(read(3,current),ADD(13,3,16),readNum(13),write(current,12), Call(helper, Seq(read(3, current), read(3,begin)))),  //recur on the left tree
                  ADD(18, 15, 0),
                  block(read(13,current),ADD(13, 13, 16),ADD(13,13,16),readNum(13), write(current, 12), Call(helper, Seq(read(3, current), read(3,begin)))), // call the recursion on the right tree
                  SLT(19, 18, 15),
                  BEQ(19, 0, 2),
                  ADD(3, 15, 0),
                  BEQ(0, 0, 1),
                  ADD(3, 18, 0)

                )
              )
            )
          )


      )


    val procedure = new Procedure("tree height", Seq(p1, p2), Seq())
    procedure.code = block(
      negOne, // $10 = -1
      posOne, // $16 = 1
      Call(helper, Seq(ADD(3, 0, 0), read(3,p1)))
    )

   Seq(procedure,helper)
  }
}
