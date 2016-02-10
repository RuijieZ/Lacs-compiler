import cs241e.assignments.{Assembler, A1, Lacs}
import cs241e.mips.Word

def prog =
  """
    def main(a:Int, b:Int): Int = {
      a+b
    }
  """
val compiled = Lacs.compile(prog)
val endState = A1.loadAndRun(compiled.words,
    Word(Assembler.encodeSigned(5)),
    Word(Assembler.encodeSigned(10))
)
endState.reg(3)
