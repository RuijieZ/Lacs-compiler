package cs241e.assignments

//import com.sun.org.apache.xpath.internal.operations.Variable

//import com.sun.tools.hat.internal.model.JavaLazyReadObject
import cs241e.assignments.Assembler._
import cs241e.assignments.CodeBuilders._
import cs241e.assignments.Debugger._
import cs241e.assignments.MemoryManagement._
import cs241e.assignments.ProgramRepresentation._
import cs241e.mips._

import scala.util.control.Breaks
import scala.collection.mutable


/** Implementations of various transformations on the `Code` objects defined in `ProgramRepresentation.scala`.
  * In general, the transformations successively eliminate various types of `Code` objects by translating them
  * into sequences of simpler `Code`s, until only `Code`s directly representing machine instructions are left.
  */

object Transformations {
  /* ############################################################### */
  /* ## Assignment 2 ############################################### */
  /* ############################################################### */

  /* Before doing Assignment 2, read the code in the first half of `ProgramRepresentation.scala`
   * (up to the `Block` case class) to get an idea of how we will represent programs using the various subclasses
   * of the `Code` class.
   *
   * Hint: You can navigate to the `Code` class by Ctrl-clicking on any use of the word `Code`.
   */

  /* Complete the implementation of the following method by replacing the `???`. */

  /** Given a sequence of `Code`s that may be one of `CodeWord`, `Define`, `Use`, or `BeqBne`,
    * resolve all of the labels, and output the corresponding MIPS machine-language program
    * as a sequence of `Word`s.
    *
    * Refer to `ProgramRepresentation.scala` for documentation of the meanings of these `Code`s.
    *
    * If a label is defined multiple times or if a label is used but not defined, call
    * `error()` with an appropriate error message.
    *
    * Scala hint: Consult the Scala library documentation for classes such as Seq:
    * http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Seq
    *
    * The high-level structure of this method is given for you, but you should learn about
    * methods such as `foreach` and `flatMap` because you may want to use them yourself in
    * later assignments.
    */
  def eliminateLabels(code: Seq[Code]): Seq[Word] = {
    val labelToValue = mutable.Map[Label, Int]()

    /* First pass: fill in the `labelToValue` map with an address for each label. */
    def setLabels(): Unit = {
      var location = 0
      code.foreach {
        case Define(label) =>
          if (!labelToValue.contains(label)) {
            labelToValue.update(label,location)
          } else {
            println("the label value is " + labelToValue(label))
            error("The label " + label.toString() + " has already been defined, please try another name")
          };
        case _ => location += 4
      }
    }
    /* Helper function that gets rid of the optional  */
    def takeAwayOpt(x: Option[Int]) = x match {
      case Some(s) => s
      case None => 0
    }
    /* Second pass: replace each `Code` with an equivalent sequence of `Word`s. */
    def translate: Seq[Word] = {
      var location = 0
      code.flatMap {
        case Define(label) =>  Seq()
        case CodeWord(word) => location += 4; Seq(word)
        case Use(label) => location += 4
          if (labelToValue.contains(label)){
            val address = takeAwayOpt(labelToValue.get(label))
            val word1 = Word(encodeUnsigned(address))
            Seq(word1)
          } else {
            error("label " + label.toString() + "does not exist!")
          }
        case BeqBne(bits, label) => location += 4
          if (labelToValue.contains(label)) {
            val labelLocation = takeAwayOpt(labelToValue.get(label))
            val offsetNum = (labelLocation - location) / 4
            val last16Bits = encodeSigned(offsetNum,16)
            val word2: Word = Word(bits ++ last16Bits)
            Seq(word2)
          } else {
            error("label " + label.toString() + "does not exist!")
          }

        case _ => require(false, s"Encountered unsupported code $code."); location += 4 ; Seq() // there is no need to put code here
      }
    }

    setLabels()
    translate
  }

  /** Links two sequences of code together to form a single program. */
  def link(codes1: Seq[Code], codes2: Seq[Code]): Seq[Code] = codes1 ++ codes2

  /** Remove all `Comment`s from a sequence of `Code`s.
    *
    * Assumes that the input sequence does not contain any `Code`
    * types that are defined after `Block` in `ProgramRepresentation.scala`.
    */
  def eliminateComments(codes: Seq[Code]): Seq[Code] = {
    var newCodes: Seq[Code] = Seq()
    codes.foreach {
      case a: Define => newCodes = newCodes :+ a
      case b: CodeWord => newCodes = newCodes :+ b
      case c: Block => newCodes = newCodes :+ c
      case d: Use => newCodes = newCodes :+ d
      case e: BeqBne => newCodes = newCodes :+ e
      case _ =>
    }
    newCodes
  }

  /** Eliminate all `Block`s from a tree of `Code` by flattening the tree into a sequence of
    * `Code`s other than `Block`s.
    *
    * Assumes that the input `code` tree does not contain any `Code`
    * types that are defined after `Block` in `ProgramRepresentation.scala`.
    */

  /* create a helper function for eliminateBlocks*/
  def elHelper(s: Seq[Code]): Seq[Code] = {
    var result: Seq[Code] = Seq()
    s.foreach {
      x =>
        result = result ++ eliminateBlocks(x)
    }
    result
  }

  def eliminateBlocks(code: Code): Seq[Code] = code match {
    case Block(children) => elHelper(children)
    case _ => Seq(code)
  }

  /** Transform a `Code` tree by applying the function `fun` to transform each node of the tree of type `Code`.
    *
    * More specifically, at a given node `code`, first recursively call `mapCode` to transform each of its
    * children. Then apply `fun` on the resulting node whose children have been transformed. If the partial
    * function `fun` is not defined on this node, just return the node itself (in other words, apply the
    * identity function in cases where `fun` is not defined).
    *
    * Note: `mapCode` should handle **all** the various possible subclasses of `Code`, not only the ones that
    * we have used so far.
    */
  def mapCode(code: Code, fun: PartialFunction[Code, Code]): Code = {
    def recurse(child: Code): Code = mapCode(child, fun)

    val appliedToChildren: Code = code match {
      case Block(children) =>
        var newChildren: Seq[Code] = Seq()
        children.foreach(
          x=>
            newChildren = newChildren :+ recurse(x)
        )
        Block(newChildren)
      case WithTempVar(code, variable) => val temp = recurse(code); WithTempVar(temp,variable)
      case IfStmt(elseLabel, e1, comp, e2, thens, elses) => IfStmt(elseLabel,recurse(e1),recurse(comp),recurse(e2),recurse(thens),recurse(elses))
      case Call(procedure, args, isTail) => var newArgs: Seq[Code] = Seq()
        args.foreach {
          x =>
            newArgs = newArgs :+ recurse(x)
        }
        Call(procedure, newArgs, isTail)
      case CallClosure(procedure, args, params, isTail) =>  var newArgs: Seq[Code] = Seq()
        args.foreach {
          x =>
            newArgs = newArgs :+ recurse(x)
        }
        CallClosure(recurse(procedure), newArgs, params, isTail)
      case _ => code
    }
    if(fun.isDefinedAt(appliedToChildren)) {
      fun(appliedToChildren)
    } else {
      appliedToChildren
    }
  }

  /* ############################################################### */
  /* ## Assignment 3 ############################################### */
  /* ############################################################### */

  /** Eliminate all `VarAccess`es from a tree of `Code` by replacing them with machine language code for
    * reading or writing the relevant variable.
    *
    * To do this, we need an activation record or `frame` containing the `Variable`s used in `code` that
    * determines the address in memory where each `Variable` is stored, as an offset from the address in
    * `Reg.framePointer`. See the definition of the `Chunk` class in `MemoryManagement.scala` for details.
    *
    * Hint: the `mapCode` method is very helpful for transforming `Code` in general, and for eliminating
    * `VarAccess`es specifically.
    *
    * Scala Hint: {case va: VarAccess => ???} defines a `PartialFunction` that is defined for `VarAccess`es
    * but is not defined for any other type of `Code`. Read
    * http://www.scala-lang.org/api/current/index.html#scala.PartialFunction
    * for an explanation of `PartialFunction`.
    *
    * Assumes that the input sequence does not contain any `Code`
    * types that are defined after `VarAccess` in `ProgramRepresentation.scala`.
    */
  def eliminateVarAccessesA3(code: Code, frame: Chunk): Code = mapCode(code,
  {case va: VarAccess => frame.access( va.variable,va.register, va.read)})

  /** Given a `body` of `Code` and a `frame` of variables that it uses, generates
    * code that allocates space for the `frame` on the stack and sets `Reg.framePointer` to
    * point to it, followed by the `body`, followed by code to free the space for the `frame` from the stack.
    */
  def allocateFrameOnStack(body: Code, frame: Chunk): Code =
    block(Stack.allocate(frame),CodeWord(ADD(Reg.framePointer, Reg.allocated, 0)), body, Stack.pop)

  /** A bundle of machine language code in the form of a sequence of `Word`s and a `debugTable` for the `Debugger`. */
  case class MachineCode(words: Seq[Word], debugTable: DebugTable)

  /** Given a `Code` tree containing only `Code` types that are defined before `Block` in `ProgramRepresentation.scala`,
    * successively eliminates all `Code` types to yield just a sequence of `Word`s representing the equivalent program
    * in machine language. In addition, generates a `DebugTable` for that program.
    */
  def toMachineCode(code: Code): MachineCode = {
    val code2 = eliminateBlocks(code)
    val debugTable = createDebugTable(code2)
    val code3 = eliminateComments(code2)
    val code4 = eliminateLabels(code3)
    MachineCode(code4, debugTable)
  }

  /** Given a `Code` tree containing only `Code` types that are defined before `VarAccess` in
    * `ProgramRepresentation.scala`, successively eliminates all `Code` types to yield just a sequence of `Word`s
    * representing the equivalent program in machine language. In addition, generates a `DebugTable` for that program.
    *
    * Requires a sequence of all `Variable`s that are accessed within `code`. The generated machine language code
    * will allocate space for the variables on the stack and free it at the end.
    */
  def compilerA3(code: Code, variables: Seq[Variable]): MachineCode = {
    val frame = Chunk(variables)
    val code1 = eliminateVarAccessesA3(code, frame)
    val code2 = allocateFrameOnStack(code1, frame)
    toMachineCode(block(code2, JR(31)))
  }

  /* ############################################################### */
  /* ## Assignment 4 ############################################### */
  /* ############################################################### */

  /** Eliminate all `WithTempVar`s from a tree of `Code` by simply returning the `code` field
    * for each one.
    *
    * Return a pair of the resulting code and a sequence of the temporary `Variable`s extracted
    * from the `WithTempVar`s.
    *
    * Assumes that the input `code` tree does not contain any `Code`
    * types that are defined after `WithTempVar` in `ProgramRepresentation.scala`.
    *
    * Hint: Use `mapCode`.
    */
  var seqOfVariables: Seq[Variable] = Seq()
  def eliminateHelper:PartialFunction[Code, Code] = {
    case  WithTempVar(code, variable) =>   seqOfVariables = seqOfVariables :+ variable; code

  }

  def eliminateTempVars(code: Code): (Code, Seq[Variable]) = {
    val result: Code = mapCode(code, eliminateHelper)
    (result, seqOfVariables)
  }

  /** Eliminate all `IfStmt`s from a tree of `Code` by translating them to simpler pieces
    * of `Code`.
    *
    * Assumes that the input `code` tree does not contain any `Code`
    * types that are defined after `IfStmt` in `ProgramRepresentation.scala`.
    */

  def eliminateIfHelper: PartialFunction[Code, Code] = {
    case IfStmt(elseLabel, e1, comp, e2, thens, elses) => val lastLabel = new Label("lastLabel")
      val result = eliminateTempVars(binOp(e1,comp,e2))._1
      block(result,thens,beq(3,3,lastLabel), Define(elseLabel),elses, Define(lastLabel))
  }

  def eliminateIfStmts(code: Code): Code = mapCode(code, eliminateIfHelper)

  def compilerA4(code: Code, variables: Seq[Variable]): MachineCode = {
    val code1 = eliminateIfStmts(code)
    val (code2, tempVariables) = eliminateTempVars(code1)
    compilerA3(code2, variables ++ tempVariables)
  }

  /* ############################################################### */
  /* ## Assignment 5 ############################################### */
  /* ############################################################### */

  /** Given a list `params` of `Variables`, constructs the same number of temporary variables using
    * nested `WithTempVar`s. The provided `code` is nested inside the innermost `WithTempVar`, so that
    * it can use all of the constructed temporary variables. For each `param` whose `isPointer` is set
    * to true in the input list `params`, the temporary variable at the same position in the list that
    * is passed to `code `returned will also have its `isPointer` set.
    */
  def withTempVars( params: List[Variable],
                    code: List[Variable]=>Code,
                    accumulator: List[Variable] = List()
                    ): Code =
    params match {
      case Nil => code(accumulator.reverse)
      case param :: tail => withTempVar(temp => withTempVars(tail, code, temp :: accumulator), param.isPointer)
    }

  /** Given a set of `keys` for a map and a `function`, applies the function to each key, and stores
    * the result in a `Map` from `keys` to the `function` values.
    */
  def makeMap[A, B](keys: Seq[A], function: A=>B): Map[A, B] = keys.map(key => (key, function(key))).toMap

  /** Given a sequence of `Procedure`s, compiles the procedures to machine language. The first procedure
    * in the sequence is considered the main procedure that should be executed first in the program. This
    * is achieved by adding an extra `startProcedure` that calls the main procedure with the values
    * of registers $1 and $2 as arguments. The main procedure must have exactly two parameters to receive
    * these values.
    */


  def compilerA5(inputProcedures: Seq[Procedure]): MachineCode = {
    require(!inputProcedures.isEmpty)
    require(inputProcedures.head.parameters.size == 2)

    val procedures = startProcedure(inputProcedures.head) +: inputProcedures

    /** The `Chunk` to store the parameters of each procedure. */
    val paramChunks: Map[Procedure, Chunk] =
      makeMap(procedures, procedure => Chunk(procedure.parameters).withBaseRegister(Reg.allocated))

    /** Compile a single `procedure` to machine language. */
    def compileProcedure(procedure: Procedure): Code = {

      /** Eliminate all `Call`s from a tree of `Code` by translating them to simpler pieces
        * of `Code`.
        *
        * The general strategy for passing arguments is as follows:
        * - create temporary variables, one for each argument
        * - evaluate the arguments, storing them in the temporary variables
        * - allocate memory for the parameter `Chunk`
        * - copy the values of the temporary variables to the parameter `Chunk`
        *
        * Assumes that the input `code` tree does not contain any `Code`
        * types that are defined after `Call` in `ProgramRepresentation.scala`.
        */


      def eliminateCalls(code: Code): Code = mapCode(code, {
        case Call(procedure, arguments,isTail) =>
          val currentChunk = paramChunks(procedure)
          val allocateCode = Stack.allocate(currentChunk)
          val variablesInChunk = currentChunk.variables.toList
          withTempVars(variablesInChunk, {
            tempVars => var seqOfCode: Seq[Code] = Seq()
              //seqOfCode = seqOfCode :+ Comment("Begin function call... evaluate arguments and save in temp vars")
              for(i <- tempVars.indices) {
                seqOfCode = seqOfCode :+ arguments(i) :+ write(tempVars(i),3)
              }
              seqOfCode = seqOfCode :+ allocateCode
              //seqOfCode = seqOfCode :+ Comment("Now copy temp vars to the parameter chunk")
              for(i <- tempVars.indices) {
                seqOfCode = seqOfCode :+ read(3,tempVars(i)) :+ currentChunk.write(variablesInChunk(i),3)
              }
              seqOfCode = seqOfCode :+ CodeWord(LIS(Reg.targetPC)) :+ Use(procedure.label) :+ CodeWord(JALR(Reg.targetPC))
              Block(seqOfCode)
          })
      }
      )
      /** Adds a prologue and epilogue to the `code` of a procedure.
        *
        * The prologue assumes that when the procedure is called, `Reg.allocated` contains the address
        * of the parameter chunk for the procedure.
        *
        * The prologue:
        * - saves the address of the parameter chunk to store it into the `paramPtr` variable in the procedure `frame`
        * - allocates space for the procedure `frame` on the stack
        * - saves the caller's value of `Reg.framePointer` in the `dynamicLink` variable of the procedure `frame`
        * - sets `Reg.framePointer` to the address of the newly-allocated `frame` for the current procedure
        * - saves `Reg.savedPC` in the `savedPC` variable of the `frame`
        *
        * The epilogue:
        * - restores `Reg.savedPC` and `Reg.framePointer` from the current frame
        * - pops the current procedure's paramChunk and `frame` off the stack
        * - returns control to the caller
        */
      def addEntryExit(code: Code, frame: Chunk): Code = {
        val paramPtr: Variable = procedure.paramPtr
        val dLink : Variable = procedure.dynamicLink
        val savedPC: Variable = procedure.savedPC
        val enter = block(
          ADD(Reg.savedParamPtr, Reg.allocated,0),
          //Comment("we are entering"),
          Stack.allocate(frame),
          frame.withBaseRegister(Reg.allocated).write(savedPC, Reg.savedPC),
          frame.withBaseRegister(Reg.allocated).write(dLink, Reg.framePointer),
          ADD(Reg.framePointer, Reg.allocated,0),
          frame.write(paramPtr, Reg.savedParamPtr)
          //JALR(Reg.allocated)
        )
        val exit = block(
          //Comment("now we are exsiting"),
          frame.read(Reg.savedPC, savedPC),
          frame.read(Reg.framePointer, dLink),
          Stack.pop,
          Stack.pop,
          JR(31)
        )
        block(Define(procedure.label), enter, code, exit)
      }

      /** Eliminate all `VarAccess`es from a tree of `Code` by replacing them with machine language code for
        * reading or writing the relevant variable.
        *
        * In contrast to Assignment 3, this method needs to handle accesses not only to variables,
        * but also to the parameters of the current procedure.
        *
        * Assumes that the input sequence does not contain any `Code`
        * types that are defined after `VarAccess` in `ProgramRepresentation.scala`.
        */
      def eliminateVarAccessesA5(code: Code, frame: Chunk): Code = {
        // var codes: Seq[Code] = Seq()
        mapCode(code, {
          case va: VarAccess => val targetV = va.variable

            if (frame.variables.contains(targetV)) {
              frame.access(va.variable,va.register, va.read)
            } else {
              val paramChunk = paramChunks(procedure)
              block (frame.read(Reg.scratch, procedure.paramPtr),
                paramChunk.withBaseRegister(Reg.scratch).access(va.variable,va.register, va.read))
            }
        })
      }

      /* Main body of compileProcedure. */

      val code1 = eliminateCalls(procedure.code)
      val code2 = eliminateIfStmts(code1)
      val (code3, tempVariables) = eliminateTempVars(code2)

      val frame = Chunk(procedure.variables ++ tempVariables ++ Seq(procedure.dynamicLink, procedure.paramPtr, procedure.savedPC))

      val code4 = eliminateVarAccessesA5(code3, frame)
      addEntryExit(code4, frame)
    }

    /* Main body of compilerA5. */

    val code = block(
      Stack.allocate(Chunk(procedures.head.parameters)), // allocate parameter chunk for start procedure
      Block(procedures.map(compileProcedure))
    )
    toMachineCode(code)
  }

  def startProcedure(mainProcedure: Procedure): Procedure = {
    val ret = new Procedure("start", Seq(), Seq())
    ret.code = Call(mainProcedure, Seq(ADD(Reg.result, Reg.input1), ADD(Reg.result, Reg.input2)))
    ret
  }

  /* ############################################################### */
  /* ## Assignment 6 ############################################### */
  /* ############################################################### */

  val closureCode = new Variable("closure code")
  val closureEnvironment = new Variable("closure environment", isPointer = true)
  /** A chunk representing a closure consisting of:
    * - the address of the code of the closure
    * - the address of the frame of the enclosing environment of the closure, which will become the static link when
    *   the closure is invoke
    */
  lazy val closureChunk = Chunk(Seq(closureCode, closureEnvironment)).withBaseRegister(Reg.allocated)

  /** Given a sequence of `Procedure`s, compiles the procedures to machine language. The first procedure
    * in the sequence is considered the main procedure that should be executed first in the program. This
    * is achieved by adding an extra `startProcedure` that calls the main procedure with the values
    * of registers $1 and $2 as arguments. The main procedure must have exactly two parameters to receive
    * these values.
    */
  def compilerA6(inputProcedures: Seq[Procedure]): MachineCode = {
    require(!inputProcedures.isEmpty)
    require(inputProcedures.head.parameters.size == 2)

    val procedures = startProcedure(inputProcedures.head) +: inputProcedures

    /** The `Chunk` to store the parameters of each procedure. */
    val paramChunks: Map[Procedure, Chunk] =
      makeMap(procedures, procedure => Chunk(procedure.parameters ++ procedure.staticLink).withBaseRegister(Reg.allocated))

    /** The set of procedures whose frame needs to be allocated on the heap (instead of on the stack).
      * This includes:
      * - every procedure that is ever made into a closure
      * - recursively, every enclosing procedure that contains an inner procedure nested within it whose frame is
      *   allocated on the heap
      */


    var frameOnHeap: Set[Procedure] = Set()  // HOW DO i know if a procedure might be used as a closure.
    def fHelper(p:Procedure): Procedure = {
      mapCode(p.code, {
        case Closure(procedure) => frameOnHeap = frameOnHeap ++ Set(procedure)
          var temp = procedure
          while(temp.outer.nonEmpty) {
            frameOnHeap = frameOnHeap ++ Set(temp.outer.get)
            temp = temp.outer.get
          }
          Closure(procedure)
      })
      p
    }
    val copy = procedures
    val copy2 = copy.map(fHelper)

    /** The first phase of compilation: performs the transformations up to eliminateTempVars so that
      * the full set of variables of the procedure is known, and so that a `Chunk` can be created for the
      * frame of the procedure. Since the second phase requires a frame for every
      * procedure, the first phase must be completed for all the procedures before the second phase
      * can begin. Returns the code for the procedure and the `Chunk` for the procedure's frame.
      */
    def phaseOne(procedure: Procedure): (Code, Chunk) = {

      /** Generates the code that computes the value for the static link in a call to target.
        * Specifically, the static link should be the address of the frame in the current chain
        * of static links that corresponds to the procedure that directly encloses target.
        */
      def computeStaticLink(target: Procedure): Code = {
        require(target.outer.nonEmpty)
        val targetDepth = target.depth
        val callerDepth = procedure.depth
        var n = callerDepth - targetDepth + 1
        if (n == 0) {
          block(
            ADD(3,0,Reg.framePointer)
          )
        } else if ( n == 1){
          block(
            read(3,procedure.staticLink.get)
          )
        } else {
          var tempProcedure = procedure
          while (n > 1) {
            // follow the s link n times
            tempProcedure = procedure.outer.get
            n = n-1
          }
          block(read(3,tempProcedure.staticLink.get))
        }
      }

      /** Eliminate all `Call`s and `CallClosure`s from a tree of `Code` by translating them to simpler pieces
        * of `Code`.
        *
        * Assumes that the input `code` tree does not contain any `Code`
        * types that are defined after `CallClosure` in `ProgramRepresentation.scala`.
        */
      def eliminateCalls(code: Code): Code = {

        /** Generates the implementation of a procedure call (either direct or through a closure).
          * The general strategy is as follows:
          * - compute arguments (including static link) and allocate parameter chunk as in Assignment 5
          * - if this is a tail call
          *   - set Reg.savedPC and Reg.framePtr to the values from the current procedure's caller like
          *       in the epilogue in addEntryExit
          *   - if the target parameter chunk is on the heap
          *     - deallocate frame and parameter chunk of current procedure (if they are on the stack)
          *   - else
          *     - deallocate target parameter chunk and frame and parameter chunk of current procedure
          *         (if they are on the stack)
          *     - allocate new target parameter chunk
          *     - copy the deallocated target parameter chunk to the new target parameter chunk
          *   - endif
          * - endif
          * - transfer control to target procedure
          *
          * Hint: be careful about which base registers hold the address of the various `Chunk`s, particularly
          * the frame of the current procedure and the parameter `Chunk` of the target procedure.
          *
          * @param targetPC code to compute the address of the target procedure to be called and put it in
          *                 `Reg.targetPC`
          * @param args code to compute the arguments to be passed to the target procedure, not including
          *             the static link
          * @param paramChunk the Chunk to hold the arguments to be passed to the target procedure
          * @param staticLink code to compute the static link to be passed to the target procedure
          * @param targetOnHeap true if the paramChunk of the target procedure is to be allocated on the heap
          *                     instead of the stack
          * @param isTail true if the call is in tail position in the calling procedure
          */
        def implementCall( targetPC: Code,
                           args: Seq[Code],
                           paramChunk: Chunk,
                           staticLink: Code,
                           targetOnHeap: Boolean,
                           isTail: Boolean
                           ): Code = {
          // //        val writeSLink =  block(staticLink, paramChunk.withBaseRegister(3).write(s,3)
          //         var seqCode: Seq[Code] =  seqCode :+ staticLink :+write(paramChunk.withBaseRegister()) targetPC :+ Stack.allocate(paramChunk)
          //          if(isTail) {
          //            // how do i know the frame
          //            seqCode = seqCode :+ read(Reg.savedPC, procedure.savedPC) :+ read(Reg.framePointer, procedure.dynamicLink)
          //            if (targetOnHeap) {
          //              seqCode = seqCode :+  Stack.pop :+ Stack.pop
          //            } else {
          //              // Can I make the assumption that pop three times will do the job
          //              seqCode = seqCode :+ Stack.pop :+ Stack.pop :+ Stack.pop :+ Stack.allocate(paramChunk)
          //              // how do I copy the chunk
          //            }
          //          }
          //          seqCode = seqCode :+ CodeWord(JALR(31))
          //          Block(seqCode)
          block()
        }



        mapCode(code, {
          case Call(procedure, arguments,isTail) =>
            val currentChunk = paramChunks(procedure)
            val allocateCode = Stack.allocate(currentChunk)
            //val variablesInChunk = currentChunk.variables.toList ++ procedure.staticLink
            val variablesInChunk = procedure.parameters.toList ++ procedure.staticLink
            withTempVars(variablesInChunk, {
              tempVars => var seqOfCode: Seq[Code] = Seq()
                for(i <- arguments.indices) {
                  seqOfCode = seqOfCode :+ arguments(i) :+ write(tempVars(i),3)
                }
                //var length = arguments.length
                if (procedure.staticLink.isDefined) {
                  seqOfCode = seqOfCode :+ computeStaticLink(procedure) :+ write(tempVars.last, 3)
                  //length =  length + 1
                }
                if (!frameOnHeap(procedure)) seqOfCode = seqOfCode :+ allocateCode
                else seqOfCode = seqOfCode :+ heap.allocate(currentChunk)
                for(i <- tempVars.indices) {
                  seqOfCode = seqOfCode :+ read(3,tempVars(i)) :+ currentChunk.write(variablesInChunk(i),3)
                }
                seqOfCode = seqOfCode :+ CodeWord(LIS(Reg.targetPC)) :+ Use(procedure.label) :+ CodeWord(JALR(Reg.targetPC))
                Block(seqOfCode)
            })
          case CallClosure(closure,arguments, parameters, isTail) =>
            val staticLink = new Variable("hello world")
            val variablesInChunk = parameters.toList :+ staticLink
            val chunk = new Chunk(variablesInChunk).withBaseRegister(Reg.allocated)
            var seqCode: Seq[Code] = Seq()
            seqCode = seqCode :+ closure
            block(
              closure,
              ADD(15,0,3),                              // Store the address of the closure chunk in the $15
              withTempVars(variablesInChunk.toList, {
                tempVars => var seqOfCode: Seq[Code] = Seq()
                  for(i <- arguments.indices) {
                    seqOfCode = seqOfCode :+ arguments(i) :+ write(tempVars(i),3)
                  }
                  seqOfCode = seqOfCode :+ heap.allocate(chunk)
                  for(i <- arguments.indices) {
                    seqOfCode = seqOfCode :+ read(3,tempVars(i)) :+ chunk.withBaseRegister(6).write(variablesInChunk(i),3)
                  }
                  seqOfCode = seqOfCode :+ closureChunk.withBaseRegister(15).read(3, closureEnvironment) :+ chunk.withBaseRegister(6).write(staticLink,3)
                  Block(seqOfCode)
              }),
              closureChunk.withBaseRegister(15).read(Reg.targetPC,closureCode),
              JALR(Reg.targetPC))
        })



      }

      /** Eliminate all `Closure`s from a tree of `Code` by translating them to simpler pieces of `Code`.
        *
        * As given, this method just returns the `code` unchanged. When you implement handling of closures in
        * Assignment 6, you will change the method body to actually eliminate `Closure`s.
        *
        **/
      def eliminateClosures(code: Code): Code = {
        mapCode(code, {
          case Closure(procedure) => block(
            LIS(10),
            Use(procedure.label),
            heap.allocate(closureChunk),
            closureChunk.withBaseRegister(6).write(closureCode,10),
            computeStaticLink(procedure),
            closureChunk.withBaseRegister(6).write(closureEnvironment,3),
            ADD(Reg.result,0,Reg.allocated))}
        )
      }

      /** Find `Call`s that appear in tail position in their containing procedure. Replace each one with the same
        * `Call` but with `isTail` set to `true`.
        *
        * Hint: If a call in tail position is to a procedure nested within the current one, is it safe to do
        * a tail call?
        *
        * As given, this method just returns the `code` unchanged. When you implement handling of tail calls in
        * Assignment 6, you will change the method body to actually detect tail calls.
        */
      def detectTailCalls(code: Code): Code = code

      /* Main body of phaseOne. */

      val code1 = eliminateClosures(procedure.code)
      val code2 = detectTailCalls(code1)
      val code3 = eliminateCalls(code2)
      val code4 = eliminateIfStmts(code3)
      val (code5, tempVariables) = eliminateTempVars(code4)

      val frame = Chunk(procedure.variables ++ tempVariables ++ Seq(procedure.dynamicLink, procedure.paramPtr, procedure.savedPC))

      (code5, frame)
    }

    val phaseOneResults: Map[Procedure, (Code, Chunk)] = makeMap(procedures, phaseOne)

    def phaseTwo(procedure: Procedure): Code = {
      val (code, frame) = phaseOneResults(procedure)

      /** Adds a prologue and epilogue to the `code` of a procedure.
        *
        * Hint: The implementation of this method is starts out the same as in Assignment 5, and you can copy
        * your code from there. When you implement closures, you need to modify it so that the frame
        * and parameters are on the heap if `frameOnHeap(procedure)` is true.
        *
        * The prologue assumes that when the procedure is called, `Reg.allocated` contains the address
        * of the parameter chunk for the procedure.
        *
        * The prologue:
        * - saves the address of the parameter chunk to stores it into the `paramPtr` variable in the procedure `frame`
        * - allocates space for the procedure `frame` on the stack or heap
        * - saves the caller's value of `Reg.framePointer` in the `dynamicLink` variable of the procedure `frame`
        * - sets `Reg.framePointer` to the address of the newly-allocated `frame` for the current procedure
        * - saves `Reg.savedPC` in the `savedPC` variable of the `frame`
        *
        * The epilogue:
        * - restores `Reg.savedPC` and `Reg.framePointer` from the current frame
        * - pops the current procedure's paramChunk and `frame` off the stack if they are allocated on the stack
        * - returns control to the caller
        */
      def addEntryExit(code: Code, frame: Chunk): Code = {
        val paramPtr: Variable = procedure.paramPtr
        val dLink : Variable = procedure.dynamicLink
        val savedPC: Variable = procedure.savedPC
        val allocateCode = if (frameOnHeap(procedure)) heap.allocate(frame)
        else Stack.allocate(frame)
        val enter = block(
          ADD(Reg.savedParamPtr, Reg.allocated,0),
          allocateCode,
          frame.withBaseRegister(Reg.allocated).write(savedPC, Reg.savedPC),
          frame.withBaseRegister(Reg.allocated).write(dLink, Reg.framePointer),
          ADD(Reg.framePointer, Reg.allocated,0),
          frame.write(paramPtr, Reg.savedParamPtr)
        )
        val pop = if (frameOnHeap(procedure)) {block()}
        else block(Stack.pop, Stack.pop)
        val exit = block(
          frame.read(Reg.savedPC, savedPC),
          frame.read(Reg.framePointer, dLink),
          pop,
          JR(31)
        )
        block(Define(procedure.label), enter, code, exit)
      }

      /** Eliminate all `VarAccess`es from a tree of `Code` by replacing them with machine language code for
        * reading or writing the relevant variable.
        *
        * In contrast to Assignment 5, this method handles accesses to variables (and parameters) outside the currently
        * executing procedure, but in one of the outer procedures within which the current procedure is
        * nested. To do this, look up the chain of static links to find the frame (and its associated parameters)
        * of the procedure in which the variable is defined.
        *
        * Assumes that the input sequence does not contain any `Code`
        * types that are defined after `VarAccess` in `ProgramRepresentation.scala`.
        */
      def eliminateVarAccesses(code: Code): Code = {
        mapCode(code, {
          case VarAccess(register, variable, read) =>
            var temp = procedure
            var seqCode = Seq[Code]()
            if (!temp.outer.nonEmpty) {                                            // if there is no outer procedure
              if (phaseOneResults(temp)._2.withBaseRegister(Reg.framePointer).variables.contains(variable)) {
                return phaseOneResults(temp)._2.withBaseRegister(Reg.framePointer).access(variable,register, read)
              } else {
                val paramChunk = paramChunks(temp)
                return block (phaseOneResults(temp)._2.withBaseRegister(Reg.framePointer).read(Reg.scratch, temp.paramPtr),
                  paramChunk.withBaseRegister(Reg.scratch).access(variable,register,read))
              }
            } else {
              seqCode = seqCode :+ CodeWord(ADD(11, 0, Reg.framePointer))
            }
            val loop = new Breaks
            loop.breakable(
              while (temp.outer.nonEmpty) {
                if (phaseOneResults(temp)._2.withBaseRegister(11).variables.contains(variable)) {
                  loop.break()
                } else if (paramChunks(temp).withBaseRegister(4).variables.contains(variable)) {
                  loop.break()
                } else {
                  seqCode = seqCode :+ phaseOneResults(temp)._2.withBaseRegister(11).read(4, temp.paramPtr)
                  seqCode = seqCode :+ paramChunks(temp).withBaseRegister(4).read(11, temp.staticLink.get)
                  temp = temp.outer.get
                }
              }
            )

            if (phaseOneResults(temp)._2.withBaseRegister(11).variables.contains(variable)) {
              return Block(seqCode :+ phaseOneResults(temp)._2.withBaseRegister(11).access(variable,register, read))
            } else {
              val paramChunk = paramChunks(temp)
              return Block(seqCode :+ block (phaseOneResults(temp)._2.withBaseRegister(11).read(Reg.scratch, temp.paramPtr),
                paramChunk.withBaseRegister(Reg.scratch).access(variable,register,read)))
            }
        }
        )
      }

      /* Main body of phaseTwo. */

      val code1 = eliminateVarAccesses(code)
      val code2 = addEntryExit(code1, frame)
      code2
    }

    /* Main body of compilerA6. */

    val code = block(
      heap.initCode,
      Stack.allocate(Chunk(procedures.head.parameters)), // allocate parameter chunk for start procedure
      Block(procedures.map(phaseTwo))
    )
    toMachineCode(code)
  }

}

//val wordCount = Word(encodeUnsigned(arguments.size))
//val word1 = Word(encodeUnsigned(1))
//val assign = block(LIS(10),word1,LIS(11),wordCount)
//seqOfCode = seqOfCode :+ closureChunk.withBaseRegister(15).read(3, closureEnvironment)
//seqOfCode = seqOfCode :+ write(tempVars.last, 3)











