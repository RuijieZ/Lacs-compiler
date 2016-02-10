package cs241e.assignments

import cs241e.mips._

import scala.annotation.tailrec

/** An assembler that generates machine language words representing MIPS instructions. */

object Assembler {

  /* ## Assignment 1 */

  /* Complete the implementation of the following methods by replacing the `???`. */

  /** Given a sequence of bits, interpret it as an unsigned binary number and return the number.
    *
    * Scala hint: Consult the Scala library documentation for classes such as Seq:
    * http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Seq
    **/
  def decodeUnsigned(bits: Seq[Boolean]): Long = {
    val len = bits.size
    var result: Long = 0
    for (x <- 0 to len - 1)
    {
      if (bits(x))
      {
        result += twoTo(len - x - 1)
      }
    }
    result
  }

  /** Given a sequence of bits, interpret it as a signed two's-complement binary number and return the number. */
  def decodeSigned(bits: Seq[Boolean]): Long = {
    require(bits.size > 0)
    var result: Long = 0
    val len= bits.size
    for (x <- 0 to len-1)
    {
       //println(x)
       if (bits(x))
       {
         if (x == 0)
         {
           result -= twoTo(len-1)
         }
         else
         {
           result += twoTo(len - x -1 )
         }
       }
    }
    result
  }

  /** Given a non-negative number `i`, encode it as an unsigned binary number using the number of bits
    * specified by `bits`.
    *
    * Scala hint: The `bits: Int = 32` specifies `32` as the default value of bits. When calling this method, one
    * can specify the number of bits explicitly (e.g. `encodeUnsigned(42, 8)`), or leave it unspecified
    * (e.g. `encodeUnsigned(42)`), in which case the default value of 32 will be used.
    **/
  def encodeUnsigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(i >= 0)
    require(i < twoTo(bits))
    var sequence: Seq[Boolean] = List()
    var num = i
    var len = 0
    while (num != 0)
    {
      len = len + 1
      val remainder = num % 2

      if (remainder == 1)
      {
        sequence = sequence :+ true
      }
      else
      {
        sequence = sequence :+ false
      }
      num = num / 2
    }
    var sequence3:Seq[Boolean] = List()
    // the list is reversed, the following for loop will reverse it back

    for(x <- 0 to len -1)
    {
      sequence3 = sequence3 :+ sequence(len-x-1)
    }


    if (sequence3.size == bits)
    {
      sequence3
    }
    else
    {
      var sequence2: Seq[Boolean] = List()
      for(x <- 1 to bits - sequence3.size)
      {
        sequence2 = sequence2 :+ false
      }
      for (x <- sequence3)
      {
        sequence2 = sequence2 :+ x
      }
      sequence2
    }
  }

  /** Given a number `i`, encode it as a signed two's-complement binary number using the number of bits
    * specified by `bits`.
    **/
  def encodeSigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(i >= -twoTo(bits-1))
    require(i < twoTo(bits-1))
    var resultSeq: Seq[Boolean] = List()
    if (i >= 0)
    {
      return encodeUnsigned(i,bits)
    }
    else
    {
      val tempNum = -i
      val tempSeq = encodeUnsigned(tempNum,bits)
      var firstOneFound = false
      for (x <- 0 to bits -1)
      {
        if (tempSeq(bits - x - 1))  // found a one
        {
          if (firstOneFound)
          {
            resultSeq = resultSeq :+ false   // should reverse it
          }
          else
          {
            resultSeq = resultSeq :+ true // first one should not be touched
            firstOneFound = true
          }
        }
        else  // found a zero
        {
          if (firstOneFound)
          {
            resultSeq = resultSeq :+ true   // should reverse it
          }
          else
          {
            resultSeq = resultSeq :+ false   //  should stay the same
          }
        }
        // now everything is good except the list is in the reverse order. now we need to reverse this list and return it

      }  // end of for loop
    }
    var sequence: Seq[Boolean] = List()
    for(x <- 0 to bits -1)
    {
      sequence = sequence :+ resultSeq(bits-1-x)
    }
    sequence

  }


  /* Before continuing Assignment 1, go to `A1.scala` and complete the methods there. Then return here and implement
   * the following.
   */

  

  /* Each of the following methods should encode the corresponding MIPS machine language instruction as a 32-bit `Word`.
   *
   * Hint: One way to create a word is from a sequence of 32 Booleans.
   * One way to create a sequence of Booleans is using Bits.
   *
   * For example:
   * `val fourBits = Seq(true, false, true, false)`
   * `val moreBits = Bits("0101")`
   * `val eightBits = fourBits ++ moreBits`
   * `val word = Word(eightBits ++ eightBits ++ eightBits ++ eightBits)`
   */

  /* Hint: You may implement additional helper methods if you wish to factor out common code. */



  def ADD(d: Int, s: Int, t: Int = 0): Word = {
    val firstPart = Seq(false, false, false, false, false, false)
    val lastPart = Seq(false, false, false, false, false, true, false, false, false, false, false)
    Word(firstPart ++ encodeUnsigned(s,5) ++ encodeUnsigned(t,5) ++ encodeUnsigned(d,5) ++ lastPart)
  }

  def SUB(d: Int, s: Int, t: Int): Word = {
    val firstPart = Seq(false, false, false, false, false, false)  //  00 0000
    val lastPart = Seq(false, false, false, false, false, true, false, false, false, true, false)  // 000 0010 0010
    Word(firstPart ++ encodeUnsigned(s,5) ++ encodeUnsigned(t,5) ++ encodeUnsigned(d,5) ++ lastPart)
  }

  def MULT(s: Int, t: Int): Word = {
    val firstPart = Seq(false, false, false, false, false, false)  // 0000 00
    val lastPart = Seq(false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false) // 0000 0000 0001 1000
    Word(firstPart ++ encodeUnsigned(s,5) ++ encodeUnsigned(t,5) ++ lastPart)
  }

  def MULTU(s: Int, t: Int): Word = {
    val firstPart = Seq(false, false, false, false, false, false)  // 0000 00
    val lastPart = Seq(false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, true) // 0000 0000 0001 1001
    Word(firstPart ++ encodeUnsigned(s,5) ++ encodeUnsigned(t,5) ++ lastPart)
  }

  def DIV(s: Int, t: Int): Word = {
    val firstPart = Seq(false, false, false, false, false, false)  // 0000 00
    val lastPart = Seq(false, false, false, false, false, false, false, false, false, false, false, true, true, false, true, false) // 0000 0000 0001 1010
    Word(firstPart ++ encodeUnsigned(s,5) ++ encodeUnsigned(t,5) ++ lastPart)
  }

  def DIVU(s: Int, t: Int): Word = {
    val firstPart = Seq(false, false, false, false, false, false) // 0000 00
    val lastPart = Seq(false, false, false, false, false, false, false, false, false, false, false, true, true, false, true, true) // 0000 0000 0001 1011
    Word(firstPart ++ encodeUnsigned(s,5) ++ encodeUnsigned(t,5) ++ lastPart)
  }

  def MFHI(d: Int): Word = {
    val firstPart = Seq(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false) // 0000 0000 0000 0000
    val lastPart = Seq(false, false, false, false, false, false, true, false, false, false, false)  // 000 0001 0000
    Word(firstPart ++ encodeUnsigned(d,5)  ++ lastPart)
  }

  def MFLO(d: Int): Word = {
    val firstPart = Seq(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)
    val lastPart = Seq(false, false, false, false, false, false, true, false, false, true, false)
    Word(firstPart ++ encodeUnsigned(d,5)  ++ lastPart)
  }

  def LIS(d: Int): Word = {
    val firstPart = Seq(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)
    val lastPart = Seq(false, false, false, false, false, false, true, false, true, false, false)
    Word(firstPart ++ encodeUnsigned(d,5)  ++ lastPart)
  }
  def LW(t: Int, i: Int, s: Int): Word = {
    val firstPart = Seq(true, false, false, false, true, true)
    Word(firstPart ++ encodeUnsigned(s,5) ++ encodeUnsigned(t,5) ++ encodeSigned(i,16))
  }

  def SW(t: Int, i: Int, s: Int): Word = {
    val firstPart = Seq(true, false, true, false, true, true)
    Word(firstPart ++ encodeUnsigned(s,5) ++ encodeUnsigned(t,5) ++ encodeSigned(i,16))
  }

  def SLT(d: Int, s: Int, t: Int): Word = {
    val firstPart = Seq(false, false, false, false, false, false)
    val lastPart = Seq(false, false, false, false, false, true, false, true, false, true, false)
    Word(firstPart ++ encodeUnsigned(s,5) ++ encodeUnsigned(t,5) ++ encodeUnsigned(d,5) ++ lastPart)
  }

  def SLTU(d: Int, s: Int, t: Int): Word = {
    val firstPart = Seq(false, false, false, false, false, false)
    val lastPart = Seq(false, false, false, false, false, true, false, true, false, true, true)
    Word(firstPart ++ encodeUnsigned(s,5) ++ encodeUnsigned(t,5) ++ encodeUnsigned(d,5) ++ lastPart)
  }

  def BEQ(s: Int, t: Int, i: Int): Word = {
    val firstPart = Seq(false, false, false, true, false, false)
    Word(firstPart ++ encodeUnsigned(s,5) ++ encodeUnsigned(t,5) ++ encodeSigned(i,16))
  }

  def BNE(s: Int, t: Int, i: Int): Word = {
    val firstPart = Seq(false, false, false, true, false, true)
    Word(firstPart ++ encodeUnsigned(s,5) ++ encodeUnsigned(t,5) ++ encodeSigned(i,16))
  }

  def JR(s: Int): Word = {
    val firstPart = Seq(false, false, false, false, false, false)
    val SecondlastPart = Seq(false, false, false, false,  false, false, false, false,  false, false, false, false,  false, false, false, false,  false)
    val lastPart = Seq(true, false, false, false)
    val s1 = s.toLong
    Word(firstPart ++ encodeUnsigned(s1,5) ++ SecondlastPart ++ lastPart)
  }

  def JALR(s: Int): Word = {
    val firstPart = Seq(false, false, false, false, false, false)
    val SecondlastPart = Seq(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)
    val lastPart = Seq(true, false, false, true)
    Word(firstPart ++ encodeUnsigned(s,5) ++ SecondlastPart ++ lastPart)
  }
}
