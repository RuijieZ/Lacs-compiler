import cs241e.mips._

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

var result = encodeSigned(-2,5)
println(result)
Seq(1,2,3).foreach{
  x =>
    println(x)
}
Seq(1,2,3).map{
  x =>
    x+1
}

Seq(1,2,3).flatMap {
  x => Seq(x,x)
}

Seq(1,2,3).flatMap{
  x =>
    if(x<2) Seq()
    else Seq(x)
}


Seq(1,2,3).filter{
  x => x < 2
}
import cs241e.assignments._
import Assembler._
import cs241e.mips
import mips._
import ProgramRepresentation._
import CodeBuilders._
import Transformations._

def loadConstant(reg: Int, const: Int) = block(
  LIS(reg),
  Word(encodeUnsigned(const))
)

def cst(const: Int) = loadConstant(3, const)

val variable = new Variable("variable")

val code2 = block(
  ifStmt(binOp(cst(3), plus, cst(5)), gtCmp, cst(7),
    assign(variable, cst(42)),
    assign(variable, cst(64))
  ),
  read(3, variable)
)
val machineCode = compilerA4(code2, Seq(variable))
machineCode.words.foreach(println)
val startState = A1.setMem(machineCode.words)
val endState = CPU.run(startState)
println(machineCode.debugTable)
val endState2 = Debugger.debug(startState, machineCode.debugTable)
machineCode.words.map(Debugger.disassemble).foreach(println)