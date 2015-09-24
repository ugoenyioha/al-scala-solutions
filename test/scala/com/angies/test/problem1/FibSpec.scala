package com.angies.test.problem1

import com.angies.problem1.Fibonacci
import org.scalatest._

class FibSpec extends FlatSpec with Matchers {
  "fib(1)" should "equal 1" in {
    Fibonacci.fib(1) should equal(1)
    Fibonacci.tailFib(1) should equal(1)
    Fibonacci.funcFib(1) should equal(1)
  }
  it should "be the case that fib(2) should equal 1" in {
    Fibonacci.fib(2) should equal(1)
    Fibonacci.tailFib(2) should equal(1)
    Fibonacci.funcFib(2) should equal(1)
  }
  it should "be the case that fib(10) should equal 55" in {
    Fibonacci.fib(10) should equal(55)
    Fibonacci.tailFib(10) should equal(55)
    Fibonacci.funcFib(10) should equal(55)
  }
}
