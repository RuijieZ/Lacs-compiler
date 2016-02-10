package cs241e.assignments

import cs241e.scanparse.DFAs._

object A7 {
  /** A sample DFA with alphabet {0,1} that recognizes binary integers that have no useless (leading) zeroes. */
  val binaryNumbers = DFA(
    alphabet = "01".toSet,
    states = Set("start", "0", "not0"),
    start = "start",
    accepting = Set("0", "not0"),
    transition = {
      case ("start", '0') => "0"
      case ("start", '1') => "not0"
      case ("not0", _) => "not0"
    })

  /** A DFA with alphabet {0,1} that recognizes binary integers that have no useless (leading) zeroes
    * and are not divisible by 3.
    */
  val notDiv3 = DFA(
    alphabet = "01".toSet,
    states = Set("start", "0","rem1","rem2", "divisible"),
    start = "start",
    accepting = Set( "rem1", "rem2"),
    transition = {
      case ("start", '1') => "rem1"
      case ("rem1", '1') => "divisible"
      case ("rem1", '0') => "rem2"
      case ("rem2", '0') => "rem1"
      case ("rem2", '1') => "rem2"
      case ("divisible", '0') => "divisible"
      case ("divisible", '1') => "rem1"

    })

  /** A DFA with alphabet {0,1} that recognizes binary integers that have no useless (leading) zeroes
    * and are not divisible by 2 or by 3.
    */
  val notDiv23 = DFA(
    alphabet = "01".toSet,
    states = Set("0", "start", "divisible", "rem1", "rem2", "rem3", "rem4", "rem5"),
    start = "start",
    accepting = Set( "rem1","rem5"),
    transition = {
      case ("start", '1') => "rem1"
      case ("rem1", '0') => "rem2"
      case ("rem1", '1') => "rem3"
      case ("rem2", '0') => "rem4"
      case ("rem2", '1') => "rem5"
      case ("rem3", '0') => "divisible"
      case ("rem3", '1') => "rem1"
      case ("rem4", '0') => "rem2"
      case ("rem4", '1') => "rem3"
      case ("rem5", '0') => "rem4"
      case ("rem5", '1') => "rem5"
      case ("divisible", '0') => "divisible"
      case ("divisible", '1') => "rem1"
    })

  /** A DFA that recognizes a decimal number between -128 and 127 inclusive, with no useless zeroes.
    * (Zeroes are required and only permitted if removing them changes the meaning of the number.)
    * The alphabet symbols are {0,1,2,3,4,5,6,7,8,9,-}.
    */
  val decimalNumber = DFA(
    alphabet = "-0123456789".toSet,
    states = Set("start", "0","-", "-1", "+1","-12","12","-11","-10","10","11","inRange", "twoD", "oneD"),
    start = "start",
    accepting = Set("0","inRange","oneD", "twoD", "10","11","12","-10","-11","-12","+1","-1"),
    transition = {
      // begin the first stage
      case ("start",'0') => "0"
      case ("start", '1') => "+1"
      case ("start", '-') => "-"
      case ("-", '1') => "-1"
      case ("start"|"-", '2'|'3'|'4'|'5'|'6'|'7'|'8'|'9') => "oneD"
      // begin the second stage
      case("oneD", '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'0') => "twoD"
      case ("+1", '2') => "12"
      case ("+1", '1') => "11"
      case ("+1", '0') => "10"
      case ("-1", '2') => "-12"
      case ("-1", '1') => "-11"
      case ("-1", '0') => "-10"
      case ("+1", '3'|'4'|'5'|'6'|'7'|'8'|'9') => "twoD"
      case ("-1", '3'|'4'|'5'|'6'|'7'|'8'|'9') => "twoD"

      // begin the third stage
      case ("10"|"11"|"-10"|"-11", '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'0') => "inRange"
      case ("12", '1'|'2'|'3'|'4'|'5'|'6'|'7'|'0')  => "inRange"
      case ("-12", '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'0')  => "inRange"


    }
  )

  /** A DFA with alphabet {a, b, c} that recognizes any string that contains all three letters in
    * alphabetical order (i.e. "abc"), possibly interspersed with more letters. For example, "acbac"
    * and "cbacbacba" are in the language, but not "acba".
    */
  val abc = DFA(
    alphabet = "abc".toSet,
    states = Set("start","hasA","hasB","hasAll"),
    start = "start",
    accepting = Set("hasAll"),
    transition = {
      case ("start", 'a') => "hasA"
      case ("start", 'b'|'c') => "start"
      case ("hasA", 'a'|'c') => "hasA"
      case ("hasA", 'b') => "hasB"
      case ("hasB" ,'a'|'b') => "hasB"
      case ("hasB", 'c') => "hasAll"
      case ("hasAll", _) => "hasAll"
    }
  )

  /** A DFA that recognizes any string from the alphabet {a,b,c} containing abc as a substring. */
  val abcSubstring = DFA(
    alphabet = "abc".toSet,
    states = Set("start", "hasA", "hasAB", "hasAll"),
    start = "start",
    accepting = Set("hasAll"),
    transition = {
      case ("start", 'a') => "hasA"
      case ("start", 'b'|'c') => "start"
      case ("hasA", 'b') => "hasAB"
      case ("hasA", 'a') => "hasA"
      case ("hasA", 'c') => "start"
      case ("hasAB", 'a') => "hasA"
      case ("hasAB", 'b') => "start"
      case ("hasAB", 'c') => "hasAll"
      case ("hasAll", _)  => "hasAll"
    }
  )
}
