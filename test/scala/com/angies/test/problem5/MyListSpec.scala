package com.angies.test.problem5


import com.angies.problem5.MyList.MyList
import com.angies.problem5._
import org.scalatest.{FlatSpec, Matchers, FunSuite}

/**
 * Created by uenyioha on 10/28/14.
 */
class MYListSpec extends FlatSpec with Matchers {
  "MyList API functions" should "return expected values" in  {
    MyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).filter(_ % 2 == 0) should equal (MyList(2, 4, 6, 8, 10))
    MyList(1, 2, 3, 4, 5).foldLeft(1)(_ * _) should equal(120)
    MyList(1, 2, 3, 4, 5).zip(MyList(1, 2, 3, 4, 5)) should equal(MyList((1, 1), (2, 2), (3, 3), (4, 4), (5, 5)))
    MyList(1, 2, 3, 4, 5, 1, 2, 3, 4, 5).distinct should equal (MyList(1, 2, 3, 4, 5))
    MyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).groupBy(_ % 2) should equal (Map(0 -> MyList(2, 4, 6, 8, 10), 1 -> MyList(1, 3, 5, 7, 9)))
  }
}