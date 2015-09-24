package com.angies.problem1

import scala.annotation.tailrec

/**
 * Created by uenyioha on 10/26/14.
 *
 * 1. Implement fibonacci(n: Int) : BigInt naively (done)
 * 2. What resources does the naive implementation consume (stack)
 * 3. Implement fibonacci with O(n) (done)
 * 4. Scala collection well suited to solving this problem (list, stream)
 */
object Fibonacci {

  /**
   * Naive recursive implementation with pattern matching.
   * Recursive implementation consumes stack. Om Nom Nom...
   * @param n Index
   * @return Fibonacci Number
   */
  def fib(n : Int) : BigInt = n match {
    case 1 => BigInt(1)
    case 2 => BigInt(1)
    case x if x > 2 => fib(x - 1) + fib(x - 2)
  }

  /**
   * Tail recursive implementation. O(n) complexity.
   * Converted to imperative while loop.
   * @param n Index
   * @return Fibonacci Number
   */
  def tailFib(n: Int) : BigInt = {
    @tailrec
    def loop(n: Int, prev: BigInt, curr: BigInt): BigInt=
      if (n == 0) prev
      else loop(n - 1, curr, prev + curr)

    loop(n, 0, 1)
  }


  /**
   * Functional implementation using streams.
   * fibs describes fibonacci sequence. infinite komputation
   * @param n Index
   * @return Fibonacci number
   */
  def funcFib(n: Int) : BigInt = {
    lazy val fibs: Stream[BigInt] = 1 #:: fibs.scanLeft(BigInt(1))(_ + _)
    fibs.take(n).last
  }

}
