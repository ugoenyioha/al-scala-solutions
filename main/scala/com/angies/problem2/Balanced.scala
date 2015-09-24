package com.angies.problem2

/**
 * Created by uenyioha on 10/26/14.
 *
 * 1. Implement function isEverythngBalanced using any approach. (done)
 * 2. Implement it recursively and immutably (done).
 * 3. Implement it non-recursively and immutably (non recursive + mutable) (partial)
 *
 */
object Balanced {

  /**
   * Recursive implementation with stack. Also immutable.
   * Early termination in one corner case with return.
   * @param chars String to check if balanced
   * @return true if string is balanced, false if not.
   */
  def isEverythingBalanced(chars: String) : Boolean = {
    val open = Set('{', '[', '(')
    val close = Set('}', ']', ')')

    def isClosingForOpen(openCh: Char, closeCh: Char) : Boolean = {
      val oc = open.zip(close)
      oc((openCh, closeCh))
    }

    def balance(chars: List[Char], stack : List[Char]) : Boolean = (chars, stack) match {
      case (Nil, s) => s.isEmpty
      /* corner case to handle any string starting with a close paren with immediate termination as it can never be balanced*/
      case ((h :: t, Nil)) if close(h) => return false
      case ((h :: t , s)) if open(h) => balance(t, h :: s)
      case ((h :: t, sh :: st)) if close(h) =>
        if (isClosingForOpen(sh, h)) balance(t, st) else balance(t, sh :: st)
      case ((_ :: t, s)) => balance(t, s)
    }

    balance(chars.toList, Nil)
  }

  /**
   * This implementation is non-recursive but is mutable. Still thinking about this one.
   * @param chars String to check if balanced
   * @return true if string is balanced, false if not.
   */
  def isEverythingBalancedAlt(chars: String) : Boolean = {

    val open = Set('{', '[', '(')
    val close = Set('}', ']', ')')

    var stack : scala.collection.mutable.MutableList[Char] = scala.collection.mutable.MutableList[Char]()

    def isClosingForOpen(openCh: Char, closeCh: Char) : Boolean = {
      val oc = open.zip(close)
      oc((openCh, closeCh))
    }

    chars.foreach{
      case (x) if open(x) => stack.+=:(x)
      case (x) if close(x) => if (!stack.isEmpty && isClosingForOpen(stack.head, x))
        stack = stack.drop(1) else stack.+=:(x)
    }

    stack.isEmpty
  }
}
