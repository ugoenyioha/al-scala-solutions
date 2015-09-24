package com.angies.problem5.FLMyList

import com.angies.problem5._

/**
 * Created by uenyioha on 10/28/14.
 *
 * 1. Implement ONLY foldLeft recursively in FLMyList; implement other methods in terms of foldLeft where possible (done)
 * 2. BigO(1) append and prepend (when amortized) - soln. use an array (no-time :-<)
 *
 */
sealed trait FLMyList[+A] {

  def reverse = foldLeft[FLMyList[A]](Nil: FLMyList[Nothing])((acc, h) => Cons(h, acc))

  def map[B](f: A => B): FLMyList[B] =
    foldLeft[FLMyList[B]](Nil : FLMyList[Nothing])((acc, h) => Cons(f(h), acc)).reverse

  def filter(f: A => Boolean): FLMyList[A] =
    foldLeft[FLMyList[A]](Nil: FLMyList[Nothing])((acc, h) => if (f(h)) Cons(h, acc) else acc).reverse

  def zip[B](that: FLMyList[B]): FLMyList[(A, B)] = foldLeft((that, Nil: FLMyList[(A, B)]))((acc, a) =>
    acc match {
      case ((Cons(b, bs), result)) => (bs, Cons((a, b), result))
      case ((Nil, result)) => (Nil, result)
    }
  )._2.reverse

  def foldLeft[B](z: B)(f: (B, A) => B) : B

  def groupBy[B](f: A => B): Map[B, FLMyList[A]] =
    foldLeft[Map[B, FLMyList[A]]](Map[B, FLMyList[A]]()){
      (m, value) =>
        val k = f(value)
        m + Tuple2(k, Cons(value, if (m contains k) m(k) else Nil))
    }

  def distinct(implicit set: Set[Any]) : FLMyList[A] = foldLeft((set, Nil: FLMyList[A]))((acc, h) =>
    acc match {
      case ((seen, result)) => if (!seen(h)) (seen + h, Cons(h, result)) else (seen, result)
      case (seen, _) => (seen, Nil)
    }
  )._2.reverse
}

case class Cons[+A](head: A, tail: FLMyList[A]) extends FLMyList[A] {
  override def foldLeft[B](z: B)(f: (B, A) => B)= tail.foldLeft(f(z, head))(f)
}

case object Nil extends FLMyList[Nothing] {
  override def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z
}

object FLMyList {
  def apply[A](as: A*): FLMyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
