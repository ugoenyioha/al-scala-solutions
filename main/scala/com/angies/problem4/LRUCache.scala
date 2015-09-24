package com.angies.problem4

import java.security.Timestamp

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}

object LRUCache {
  def apply[K, V](fastCacheSize: Int) = {
    implicit val kOrd = new Ordering[K] { def compare(l:K,r:K) = 0 }
    new LRUCache(fastCacheSize, HashMap.empty[K, (Long, V)], HashMap.empty[K, V])
  }
}

/**
* Created by uenyioha on 10/26/14.
 *
 * This is a mutable implementation with two caches, a fast cache and a slow cache.
 * Simulates transmission between both caches.
 *
 * Design a "LRUCache with the following properties"
 *
 * 1. constructor that takes the recently used cache size (fastCches: Int) (done)
 * 2. a retrieval method of get(key: K) : Option[V] (done)
 * 3. an update method def put (key: K, value: V): Unit (done)
 * 4. the cache will not evict entries - meaning it grows unboundedly (done)
 * 5. O(1) get from fastCache - (done - fast cache)
 * 6. O(log n) get any key (implemented amortized O(1), better with HashMap - slow cache)
 * 7. O(log n) put any key (implemented amortized O(1), better with HashMap - slow cache)
 *
*/

class LRUCache[K, V] private (cacheSize: Int, fastCache : Map[K, (Long, V)],
                              slowCache: Map[K, V]) {

  implicit val trackOrd = new Ordering[(Long, K)] { def compare(left: (Long, K),right: (Long, K)) =
    left._1.toInt - right._1.toInt }

  private val trackKeys: Map[Long, K] = HashMap.empty[Long, K]

  def fastCacheSize = trackKeys.size

  var index : Long = 0

  def get(key: K): Option[V] = {


    // attempt retrieval from fast cache or slow cache as the case may be
    // policy is to update fast cache with put if retrieved from slow
    fastCache.get(key).map(_._2) orElse slowCache.remove(key).map {
       (value) => {
        put(key, value)
        value
      }
    }

  }

  def put(key: K, value: V) : Unit = {
    index += 1

    // remove any entries from the fast or slow cache
    fastCache.remove(key) map {
      case (k, v) => trackKeys.remove(k)
    } orElse slowCache.remove(key)

    // place new key and value into fast cache and update track keys
    fastCache.update(key, (index, value))

    // now update track keys
    trackKeys.update(index, key)

    // eviction required from fast cache?? - if yes then place in slow cache
    if (trackKeys.size > cacheSize)
      Try(trackKeys.min).foreach {
        case (evictIndex, evictKey) => fastCache.remove(evictKey) foreach {
          case (timestamp, v) => {
            trackKeys.remove(evictIndex)
            slowCache.update(evictKey, v)
          }
        }
      }
  }

}
