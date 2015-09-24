package com.angies.problem5.MyList

import com.angies.problem5._
/**
 * Created by uenyioha on 10/26/14.
 *
 * Implement a recursive version of:
 * map in MyList - (done)
 * filter in MyList - (done)
 * zip in MyList - (done)
 * foldLeft in MyList - (done)
 * distinct in MyList - (done)
 * groupBy in MyList - (done)
 *
 * Make them tailRecursive in TRMyList (not done - FLList uses foldLeft which is tail recursive)
 *
 */
sealed trait MyList[+A] {
  def map[B](f: A => B) : MyList[B]
  def filter(f: A => Boolean) : MyList[A]
  def zip[B](that: MyList[B]) : MyList[(A, B)]
  def foldLeft[B](z: B)(f: (B, A) => B) : B
  def distinct(implicit set: Set[Any]) : MyList[A]
  def groupBy[B](f: A => B): Map[B, MyList[A]]
}

case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A] {
  override def map[B](f: (A) => B): MyList[B] = Cons(f(head), tail map f)

  override def foldLeft[B](z: B)(f: (B, A) => B): B =
    tail.foldLeft(f(z, head))(f)

  override def filter(f: (A) => Boolean): MyList[A] = if (f(head)) Cons(head, tail filter f) else tail filter f

  override def distinct(implicit set: Set[Any]): MyList[A] =
    if (set(head))
      tail.distinct(set)
    else
      Cons(head, tail distinct (set + head))

  override def groupBy[B](f: (A) => B): Map[B, MyList[A]] = {
    val m = tail groupBy f
    val k = f(head)
    m + Tuple2(k, Cons(head, if (m contains k) m(k) else Nil))
  }

  override def zip[B](that: MyList[B]): MyList[(A, B)] = (this, that) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), t1.zip(t2))
    case (_, Nil) => Nil
  }
}

case object Nil extends MyList[Nothing] {

  override def map[B](f: (Nothing) => B): MyList[B] = Nil

  override def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z

  override def filter(f: (Nothing) => Boolean): MyList[Nothing] = Nil

  override def distinct(implicit set: Set[Any]): MyList[Nothing] = Nil

  override def groupBy[B](f: (Nothing) => B): Map[B, MyList[Nothing]] = Map[B, MyList[Nothing]]()

  override def zip[B](that: MyList[B]): MyList[(Nothing, B)] = Nil
}

object MyList {
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
