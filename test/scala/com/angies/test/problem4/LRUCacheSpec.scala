package com.angies.test.problem4

import com.angies.problem4.LRUCache
import org.scalatest.{FlatSpec, Matchers, FunSuite}

/**
 * Created by uenyioha on 10/28/14.
 */
class LRUCacheSpec extends FlatSpec with Matchers {
  "LRUCache" should "never exceed maximum size" in {
    val maxSize = 10
    val cache = LRUCache[String, Int](maxSize)
    assert(cache.fastCacheSize <= maxSize)
    cache.put("a", 0)
    assert(cache.fastCacheSize <= maxSize)
    cache.put("b", 0)
    assert(cache.fastCacheSize <= maxSize)
    cache.put("c", 0)
    assert(cache.fastCacheSize <= maxSize)
    cache.put("d", 0)
    assert(cache.fastCacheSize <= maxSize)
    cache.put("e", 0)
    assert(cache.fastCacheSize == 5)
    assert(cache.fastCacheSize <= maxSize)
    cache.put("f", 0)
    assert(cache.fastCacheSize <= maxSize)
    cache.put("g", 0)
    assert(cache.fastCacheSize <= maxSize)
    cache.put("h", 0)
    assert(cache.fastCacheSize == 8)
    assert(cache.fastCacheSize <= maxSize)
    cache.put("i", 0)
    assert(cache.fastCacheSize == 9)
    assert(cache.fastCacheSize <= maxSize)
    cache.put("j", 0)
    assert(cache.fastCacheSize == 10)
    assert(cache.fastCacheSize <= maxSize)
    cache.put("k", 0)
    assert(cache.fastCacheSize == 10)
    assert(cache.fastCacheSize <= maxSize)
  }

  it should "update exsisting elements" in {
    val maxSize = 10
    val cache = LRUCache[String, Int](maxSize)

    cache.put("a", 0)
    cache.put("a", 1)
    cache.put("a", 2)

    cache.get("a") should be(Some(2))
  }

}
