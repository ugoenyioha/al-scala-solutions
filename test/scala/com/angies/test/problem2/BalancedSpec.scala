package com.angies.test.problem2

import com.angies.problem2.Balanced
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by uenyioha on 10/28/14.
 */
class BalancedSpec extends FlatSpec with Matchers {
  "isEverythingBalancedX" should "be true for balanced parenthesis" in {
    Balanced.isEverythingBalanced("") should be(true)
    Balanced.isEverythingBalanced("([{}])") should be(true)
    Balanced.isEverythingBalanced("[]([{}[]()])") should be(true)

    Balanced.isEverythingBalancedAlt("") should be(true)
    Balanced.isEverythingBalancedAlt("([{}])") should be(true)
    Balanced.isEverythingBalancedAlt("[]([{}[]()])") should be(true)

  }

  it should "be false for unbalanced parenthesis" in {
    Balanced.isEverythingBalanced("[") should be (false)
    Balanced.isEverythingBalanced("}") should be (false)
    Balanced.isEverythingBalanced("({)}") should be (false)
    Balanced.isEverythingBalanced("}{") should be (false)
    Balanced.isEverythingBalanced("){") should be (false)

    Balanced.isEverythingBalancedAlt("[") should be (false)
    Balanced.isEverythingBalancedAlt("}") should be (false)
    Balanced.isEverythingBalancedAlt("({)}") should be (false)
    Balanced.isEverythingBalancedAlt("}{") should be (false)
    Balanced.isEverythingBalancedAlt("){") should be (false)
  }
}
