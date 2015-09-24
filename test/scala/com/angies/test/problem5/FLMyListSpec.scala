package com.angies.test.problem5


import com.angies.problem5.FLMyList.FLMyList
import com.angies.problem5._
import org.scalatest.{FlatSpec, Matchers, FunSuite}

/**
 * Created by uenyioha on 10/28/14.
 */
class FLMyListSpec extends FlatSpec with Matchers {
  "FLMyList API functions" should "return expected values" in  {
    FLMyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).filter(_ % 2 == 0) should equal (FLMyList(2, 4, 6, 8, 10))
    FLMyList(1, 2, 3, 4, 5).foldLeft(1)(_ * _) should equal(120)
    FLMyList(1, 2, 3, 4, 5).zip(FLMyList(1, 2, 3, 4, 5)) should equal(FLMyList((1, 1), (2, 2), (3, 3), (4, 4), (5, 5)))
    FLMyList(1, 2, 3, 4, 5, 1, 2, 3, 4, 5).distinct should equal (FLMyList(1, 2, 3, 4, 5))
    FLMyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).groupBy(_ % 2) should equal(Map(1 -> FLMyList(9, 7, 5, 3, 1), 0 -> FLMyList(10, 8, 6, 4, 2)))
  }
}