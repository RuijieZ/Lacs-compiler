package cs241e.assignments

import scala.annotation.tailrec
import CodeGenerator._
import cs241e.scanparse.DFAs._

/** Implementations of a DFA recognizer and a scanner. */
object Scanning {
  /** Given a DFA and an input string, determines whether the input string is in the language specified by the DFA. */
  def recognize(dfa: DFA, input: List[Char]): Boolean = {
    var startState = dfa.start
    for(i<-input.indices) {
      if(dfa.transition.isDefinedAt(startState,input(i))) {
        startState = dfa.transition(startState,input(i))
      }else {
         return false
      }
    }
    if (!dfa.accepting.contains(startState))  return false
    else return true
  }



  /** Given a DFA and an input string, splits the input string into tokens using the maximal munch algorithm.
    * Specifically, each token is the longest prefix of the remaining input string that is in the language
    * specified by the DFA. If the input string cannot be tokenized in this way, calls error() with an
    * appropriate error message.
    */
  def maximalMunchScan(dfa: DFA, input: String): Seq[Token] = {

    /** Scans a single token. More precisely, finds the longest prefix of `input` accepted by `dfa` if the `dfa`
      * starts executing in `state`.
      *
      * You may implement this method with a loop instead of recursion if you prefer, but it is more complicated
      * and more tricky to get it right.
      *
      * @param input the input string to be scanned
      * @param state the state in which the DFA is to start executing
      * @param backtrack the value to return if the DFA gets stuck (if the transition function is not defined
      *                  for the current combination of state and next symbol from the input)
      * @return a pair of the rest of the `input` after the accepted prefix has been removed and the final state
      *         of the `dfa` after it has executed from `state` on the accepted prefix
      */
    
    def scanOne(input: List[Char], state: State, backtrack: (List[Char], State) ): (List[Char], State) = {
        if (input.isEmpty) (input, state)
        else if (!dfa.transition.isDefinedAt(state,input.head)) backtrack
        else {
          val firstChar = input.head
          val rest = input.tail
          val newState = dfa.transition(state, firstChar)
          if(dfa.accepting.contains(newState)) {
            val newB = (rest,newState)
            scanOne(rest, newState, newB)
          } else {
            scanOne(rest, newState,backtrack)
          }
        }
    }


    /** Given a `list` and `rest` such that `list.tail.tail....tail eq rest` for some sequence of zero or more
      * .tail operations, returns a list containing the elements that are in `list` but not in `rest` in the
      * same order that they appear in `list`.
      *
      * Scala hint: `==` returns true if two lists contain equal elements, like equal? in Racket.
      * `eq` returns true if two lists are physically the same list, like eq? in Racket.
      */
    def listDiff[A](list: List[A], rest: List[A]): List[A] =
      if (list eq rest) Nil
      else list.head :: listDiff(list.tail, rest)

    /** The core of the maximal munch scanning algorithm. Repeatedly calls `scanOne` on the remaining
      * `input` until the input is empty, in which case it returns the list of tokens that have been
      * scanned, in reverse order. The `accumulator` accumulates the tokens in the style of generative
      * recursion. If `scanOne` makes no progress (scans the empty prefix), calls `error()` with
      * a suitable error message. Each token should contain the string of characters scanned (the lexeme)
      * and the state in which the DFA ended up after scanning the lexeme (which will be used as the kind
      * of the token).
      *
      * Hint: the `listDiff` method will be useful.
      *
      * This method can also be implemented using a loop, and you are free to do so if you prefer.
      */
    
    def recur(input: List[Char], accumulator: List[Token] = List.empty): List[Token] = {
      if(input eq List.empty) {
        accumulator
      } else {
        val temp = scanOne(input, dfa.start, (input,dfa.start))
        if (temp._1 == input ) {
          error("please double check the input string")
        } else {
          recur(temp._1,List(Token(temp._2,listDiff(input,temp._1).mkString)) ++ accumulator)
        }
      }
    }

    recur(input.toList).reverse
  }
}
