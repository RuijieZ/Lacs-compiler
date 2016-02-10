import cs241e.assignments.Assembler._
import cs241e.assignments.Transformations._
import cs241e.assignments.ProgramRepresentation._
import cs241e.assignments.CodeBuilders._
import cs241e.assignments.{A1, Reg}
import cs241e.mips._
import cs241e.assignments.Debugger

val p1 = new Variable("p1")
val p2 = new Variable("p2")
val main = new Procedure("main", Seq(p1, p2), Seq())

val i = new Variable("i")
val fact = new Procedure("fact", Seq(i), Seq())

implicit def readVariable(v: Variable): Code =
  read(Reg.result, v)

main.code = Call(fact, Seq(p1))

fact.code = ifStmt(
  i,
  leCmp,
  block(),
  LIS(3),
  binOp(i, times,
    Call(fact, Seq(binOp(i, minus, LIS(3)))))
)

val machineCode = compilerA5(Seq(main, fact))
//val finalState =
//  A1.loadAndRun(machineCode.words, Word(encodeUnsigned(10)))
//decodeUnsigned(finalState.reg(Reg.result))
println(Debugger.debug(A1.setMem(machineCode.words)
                 .setReg(1,Word(encodeUnsigned(10))),
               machineCode.debugTable))