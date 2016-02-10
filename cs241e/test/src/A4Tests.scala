import org.scalatest.FunSuite
import cs241e.Utils._
import cs241e.assignments.Assembler._
import cs241e.mips.{CPU, State, Word}
import scala.math._
import cs241e.assignments.Transformations._
import cs241e.assignments.MemoryManagement._
import cs241e.assignments.ProgramRepresentation._
import cs241e.assignments.Reg._
import cs241e.mips.CPU._
import cs241e.assignments.{Reg, A1, A4, Debugger}
import cs241e.assignments.CodeBuilders._

/**
 * Created by han on 05/10/15.
 */
class A4Tests extends FunSuite {
  test("multiplication") {
    val state = A1.loadAndRun(Seq(
      LIS(1),
      asWord(4),
      MULT(1, 1),
      MFLO(3),
      JR(31)
    ))
    println(decodeSigned(state.reg(2)))
    println(decodeSigned(state.reg(3)))
  }
  test("Unit testing for code builders") {
  }
  test("A4 last element") {
    val compiled = A4.lastElement
    val endState1 = A4.loadAndRunArray(compiled, Seq())
    assert(decodeSigned(endState1.reg(Reg.result)) == -1)
    val endState2 = A4.loadAndRunArray(compiled, Seq(asWord(1)))
    assert(endState2.reg(Reg.result) == asWord(1))

    val endState3 = A4.loadAndRunArray(compiled, Seq(asWord(1), asWord(2), asWord(3)))
    assert(endState3.reg(Reg.result) == asWord(3))

  }
  test("comp") {
    def testCmp(cmp: Label => Code, n1: Int, n2: Int, willCmpSucceed: Boolean = true): Unit = {
      val cmpFailed = new Label("comp failed")
      val prog = block(
        loadConstant(Reg.scratch, n1),
        loadConstant(Reg.result, n2),
        cmp(cmpFailed),
        loadConstant(Reg.result, 1), //cmp success
        RETURN,
        Define(cmpFailed),
        loadConstant(Reg.result, 0),
        RETURN
      )
      val compiled = compilerA4(prog, Seq())
      val endstate = A1.loadAndRun(compiled.words)
      assert((decodeSigned(endstate.reg(Reg.result)) == 1) == willCmpSucceed)
    }
    /*    testCmp(ltCmp,-1,2)
        testCmp(ltCmp,0,0,false)
        testCmp(ltCmp,3,-1,false)
        testCmp(neCmp,-1,2)
        testCmp(neCmp,0,0,false)*/
    testCmp(geCmp, 1, 0)
    testCmp(geCmp, 3, 3)
    testCmp(geCmp, -1, 0, false)
    testCmp(leCmp, -1, 0)
    testCmp(leCmp, -4, -4)
    testCmp(leCmp, 3, -2, false)
  }
  test("A4 max array") {
    val endState1 = A4.loadAndRunArray(A4.arrayMaximum, Seq(-1).map(asWord))
    assert(decodeSigned(endState1.reg(Reg.result)) == -1)
    val endState2 = A4.loadAndRunArray(A4.arrayMaximum, Seq(-1, 2, 0, -4).map(asWord))
    assert(decodeSigned(endState2.reg(Reg.result)) == 2)
    val endState3 = A4.loadAndRunArray(A4.arrayMaximum, Seq(0, 0, 0).map(asWord))
    assert(decodeSigned(endState3.reg(Reg.result)) == 0)
    val endState4 = A4.loadAndRunArray(A4.arrayMaximum, Seq(-1, -2, 3, 4, 5).map(asWord))
    assert(decodeSigned(endState4.reg(Reg.result)) == 5)
    val endState5 = A4.loadAndRunArray(A4.arrayMaximum, Seq(-1, -2, -3, -4, -5).map(asWord))
    assert(decodeSigned(endState5.reg(Reg.result)) == -1)
  }

  test("Increment") {
    val i = new Variable("counter")
    val vars = Seq(i)
    val code = block(
      loadConstant(-1),
      writeVar(i),
      increment(i),
      readVar(i)
    )
    val compiled = compilerA4(code, vars)
    val endstate = A4.loadAndRunArray(compiled, Seq())
    expectEndState(endstate, Reg.result, 0)
  }

  test("deref array") {
    val i = new Variable("array counter")
    val code = block(
      loadConstant(2),
      writeVar(i),
      derefArray(Reg.input1, i)
    )
    val vars = Seq(i)

    val compiled = compilerA4(code, vars)
    val endState = A4.loadAndRunArray(compiled, Seq(2, 3, 4).map(asWord), true)
    assert(decodeSigned(endState.reg(3)) == 4)

  }

  test("print letters") {
    //    A4.outputLetters.words.map(Debugger.disassemble).foreach(println)
    val endState = A4.loadAndRunArray(A4.outputLetters, Seq(1, 2, 3, 4).map(asWord))
    val endState1 = A4.loadAndRunArray(A4.outputLetters, Seq(1, 0, 2, 0, 3, 0).map(asWord))
  }
  test("abs") {
    val code = block(
      loadConstant(Int.MinValue),
      ABS(Reg.result)
    )
    val compiled = compilerA4(code, Seq())
    val end = A4.loadAndRunArray(compiled, Seq(), true)
    expectEndState(end, 3, Int.MaxValue)
  }
  test("print integer") {
    val code = block(
      whileLoop(loadConstant(3),eqCmp,loadConstant(3),printMipsCons(48)),
      whileLoop(block(),eqCmp,block(),whileLoop(block(),neCmp,block(),block()))
    )
    compilerA4(code,Seq())
    val initialState =
      A1.setMem(A4.printInteger.words)
        .setReg(1, asWord(1))
    CPU.run(initialState) //expect 1
    println()
    val a = new Variable("a")
    val b = new Variable("a")
   CPU.run(initialState.setReg(1, asWord(-1))) //expect -1
    println()
    CPU.run(initialState.setReg(1, asWord(0)))
    println()
    CPU.run(initialState.setReg(1, asWord(-123456789))) //expect -123
    println()
   CPU.run(initialState.setReg(1, asWord(1))) //expect 1
    println()
    CPU.run(initialState.setReg(1, asWord(1000000000))) //expect 10000000000
    println()
    CPU.run(initialState.setReg(1, asWord(-1000000))) //expect 1
    println()
    CPU.run(initialState.setReg(1, asWord(Int.MinValue ))) //expect 1
    println()
    CPU.run(initialState.setReg(1, asWord(Int.MaxValue))) //expect 1
    println()


  }

  test("IF stmt") {
    val one = loadConstant(Reg.result, 1)
    val two = loadConstant(Reg.result, 2)
    val zero = loadConstant(Reg.result, 0)
    val code = block(
      ifStmt(one, gtCmp, two, one, zero)
    )
    val compiled = compilerA4(code, Seq())
    val endState = A1.loadAndRun(compiled.words)
    assert(decodeSigned(endState.reg(Reg.result)) == 0)

  }
  test("ifstmt") {

    var testNum = 1
    println("IfStmt tests")
    def testIfStmt(code: Code, result: Long): Unit = {
      println(s"TEST $testNum:")
      testNum += 1
      val prog = compilerA4(code, Seq()).words
      println(prog.map(Debugger.disassemble(_)))
      val endState = A1.loadAndRun(prog)
      val output = decodeSigned(endState.reg(Reg.result))
      //      println(output)
      val passed = output == result
      if (passed) println("Passed!") else println("Failed")
      assert(passed)
    }
    val stmt = ifStmt(
      loadConstant(Reg.result, 10),
      eqCmp,
      loadConstant(Reg.result, 10),
      loadConstant(Reg.result, 2),
      loadConstant(Reg.result, 1))
    testIfStmt(stmt, 2)
    val stmt2 = ifStmt(
      loadConstant(Reg.result, 11),
      eqCmp,
      loadConstant(Reg.result, 10),
      loadConstant(Reg.result, 2),
      loadConstant(Reg.result, 1))
    testIfStmt(stmt2, 1)
  }
}