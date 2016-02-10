import cs241e.assignments.{Assembler, A1}
import org.scalatest.FunSuite
/* This is an example of a class for running and testing your code. Add other testing methods here
 * and/or create additional classes like this one.
 *
 * Run the tests by right-clicking on the test code and selecting Run '(test name)' from the menu.
 *
 * You can also run the tests by right-clicking on a file or directory containing tests in the Project navigator
 * on the left.
 */

class A1Tests extends FunSuite {
  test("decodeUnsigned") {

    import cs241e.assignments.ProgramRepresentation._
    import cs241e.assignments.CodeBuilders._
    import cs241e.assignments.{A1, Reg}
    import cs241e.assignments.Transformations._
    import cs241e.assignments.Assembler._
    import cs241e.mips._

    //def increaseBy2(increment: Int): (Int)=>Int = {
    //  def procedure(x: Int) = { x + increment }
    //  procedure
    //}
    //def main(a: Int, b: Int) = (increaseBy2(a))(b)

    implicit def readVariable(v: Variable): Code = read(Reg.result, v)

    val increment = new Variable("increment")
    val increaseBy = new Procedure("increaseBy", Seq(increment), Seq())
    val x = new Variable("x")
    val procedure = new Procedure("procedure", Seq(x), Seq(), Some(increaseBy))

    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b), Seq())
    val c = new Variable("c")
    increaseBy.code = Closure(procedure)

    procedure.code = binOp(x, plus, increment)
    val parameterVariable = new Variable("parameterVariable")
    main.code = CallClosure(
      Call(increaseBy, Seq(a)),
      Seq(b),
      Seq(parameterVariable)
    )
    val machineCode = compilerA6(Seq(main, increaseBy, procedure))
    def constantInt(i: Int) = Word(encodeSigned(i))
    val endState = A1.loadAndRun(machineCode.words, constantInt(42), constantInt(-5))
    println(decodeUnsigned(endState.reg(Reg.result)))
  }
}
