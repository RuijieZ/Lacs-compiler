package cs241e

import cs241e.assignments.Assembler._
import cs241e.assignments.ProgramRepresentation._
import cs241e.assignments.Reg
import cs241e.assignments.CodeBuilders._

import cs241e.mips._
/**
 * Created by han on 18/09/15.
 */
package object Utils {
  def isEven(n:Long):Boolean={
    n%2==0
  }

  def asBitStr(bits:Seq[Boolean]):String={
    bits.foldLeft(""){
      (result,x)=>
        result + (if(x)"1" else "0")
    }
  }

  def noWhiteSpace(str:String):String={
    val pattern = "\\s".r
    pattern replaceAllIn(str, "")
  }

  def wordWithoutSpace(str:String):Word={
    Word(noWhiteSpace(str))
  }


  //utilities
  def loadConstant(to:Int,constant:Int):Code = loadConstant(to,asWord(constant))
  def loadConstant(constant:Int):Code = loadConstant(Reg.result,constant)
  def loadConstant(to:Int,word:Word):Code = {
    Block(
      Seq(
        LIS(to),
        word
      )
    )
  }
  def loadLabel(to:Int,label:Label):Code = block(
    LIS(to),
    Use(label)
  )
  val RETURN = JR(31)
  def jumpLabel(label:Label):Code = block(
    loadLabel(Reg.scratch,label),
    JR(Reg.scratch)
  )

  def initMem (reg:Int,offset:Int = 0):Code = {
    toCodeWord(SW(0,offset,reg)) //load zero to a memory address specified by reg
  }

  def readVar(v:Variable):Code = {
    read(Reg.result,v)
  }
  def writeVar(v:Variable):Code = {
    write(v,Reg.result)
  }
  def addBy(reg:Int,constant:Int):Code = {
    require(reg!=Reg.scratch)
    block(
      loadConstant(Reg.scratch,constant),
      ADD(Reg.result,reg,Reg.scratch)
    )
  }
  def addBy(v:Variable,constant:Int):Code = {
    block(
      readVar(v),
      addBy(Reg.result,constant),
      writeVar(v)
    )
  }
  //increment a given register's value by 1, put results in Reg.result
  def increment(reg:Int): Code ={
    addBy(reg,1)
  }
  def increment(v:Variable):Code = {
    addBy(v,1)
  }
  def decrement(reg:Int):Code = {
    addBy(reg,-1)
  }
  def decrement(v:Variable):Code = {
    addBy(v,-1)
  }
  // leave result of absolute value to Reg.result
  def ABS(reg:Int):Code={
    withTempVar((tempVar)=>{
      block(
        write(tempVar,reg),
        ifStmt(MOV(reg,Reg.result),ltCmp,MOV(0,Reg.result),
            block(
              binOp(MOV(0,Reg.result),
                minus,
                readVar(tempVar))
            )
          ,readVar(tempVar)
        )
      )
    })
  }
  //asssumes address of array is stored in `address`, size in `size`
  def derefArray(address:Int, v:Variable,size:Int = 0):Code = block(
    Comment("deref((#of element * 4) + arraybeginAddress - 4)"),
    deref(
      block(
        Comment("$scratch = 4"),
        loadConstant(Reg.scratch,4), //
        Comment("$result = index of array"),
        readVar(v), //
        Comment("result = index * 4"),
        times,
        Comment("$result = result + begin address"),
        ADD(Reg.result,Reg.result,address) //
      ) //-4, negative offset to access the last element of the array
    )
  )
  def getIthItem(i:Variable):Code = block(
    Comment("Gets i th element in array to $result"),
    derefArray(Reg.input1,i)
  )
  def printMips(reg:Int = Reg.result,asciiOffset:Int = 0):Code = {

    require(reg!=Reg.scratch)
    val addOffset = block(
      loadConstant(Reg.scratch,asciiOffset),
      Comment(s"adding asciiOffset $asciiOffset to reg.result"),
      plus)
    block(
      if(asciiOffset !=0) {
        addOffset
      }else block(),
      Comment("Printing mips, first load print address to scratch, then store word"),
      loadConstant(Reg.scratch,CPU.printAddr),
      SW(reg,0,Reg.scratch),
      Comment("end of printing ")
    )
  }

  def printMipsCons(cons:Int):Code = block(
    Comment(s"print $cons"),
    loadConstant(cons),
    printMips()
  )


  def arrayForEach(fn:Code,end:Code = block(),vars:Seq[Variable] = Seq()):(Code,Seq[Variable]) = {
      val i = new Variable("counter")
      val variables = Seq[Variable](i) ++ vars
      val sizeOfArray = MOV(Reg.input2,Reg.result)
      val body = block(
        getIthItem(i),
        fn,
        increment(i)
      )
      val code: Code = block(
        Comment("for counter = 0 counter < size of array;counter++, " +
          "check if element is larger than currMax, reassign currMax"),
        Comment("initialize counter"),
        write(i,0),
        Comment("gets 0th element, store in $result"),
        whileLoop(readVar(i),ltCmp,sizeOfArray,body),
        end,
        RETURN
      )
    (code,variables)
  }

  def popGetVal():Code = {

    block(
      deref(MOV(Reg.stackPointer,Reg.result)),
      loadConstant(Reg.scratch,4),
      ADD(Reg.stackPointer,Reg.stackPointer,Reg.scratch)
    )
  }

  def expectEndState(endState:State,reg:Int,expected:Int):Unit = {
    val actual = decodeSigned(endState.reg(reg))
      println("actual: " + actual)
      println("expected: " + expected)
    assert(actual == expected)
  }

  def asWord(num:Int):Word={
    Word(encodeSigned(num))
  }

  def asWord(str:String)={
    wordWithoutSpace(str)
  }

  def MOV(from:Int,to:Int):Word = {
    ADD(to,from,0)
  }

}
