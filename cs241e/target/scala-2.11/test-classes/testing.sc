import cs241e.assignments.ProgramRepresentation.{CodeWord, Block}
import cs241e.assignments.{A1, Transformations, Assembler}
import cs241e.assignments.Assembler._
import cs241e.mips.Word
import cs241e.assignments.A4.loadCodeAndArray
import cs241e.assignments.A4.loadAndRunArray

/*Assembler.JR(31)
val w1 = CodeWord(ADD(4,1,3))
val w2 = CodeWord(SLT(4, 1, 2))
val w3 = CodeWord(ADD(3, 2, 0))
val w4 = CodeWord(BEQ(4, 0, 1))
val w5 = CodeWord(ADD(3, 1, 0))
val w6 = LIS(4)
val w9 = LIS(5)
val w10 = Word("00000000000000000000000000000100")
val w11 = ADD(3, 1, 5)


val a = Word("00000000000000000000000000000000")
val b = Word("00000000000000000000000000000001")

loadCodeAndArray(Seq(a),Seq(b))
loadAndRunArray(cs241e.assignments.A4.arrayMaximum,Seq(b),true)

*/