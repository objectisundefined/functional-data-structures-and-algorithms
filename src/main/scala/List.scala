package com.example.fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class ::[+A](head: A, tail: List[A]) extends List[A]

object List extends App {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else ::(as.head, apply(as.tail: _*))

  def head[A](list: List[A]): A = list match {
    case Nil => sys.error("head of empty list")
    case x :: _ => x
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("tail of empty list")
    case _ :: xs => xs
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("init of empty list")
    case _ :: Nil => Nil
    case x :: xs => ::(x, init(xs))
  }

  @annotation.tailrec
  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = list match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }
  // def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = foldRight(list, (b: B) => b)((x, g) => (b: B) => g(f(b, x)))(z)

  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }
  // def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = foldLeft(list, (b: B) => b)((g, x) => (b: B) => g(f(x, b)))(z)

  def length[A](list: List[A]): Int = foldLeft(list, 0)((acc, _) => acc + 1)
  // def length[A](list: List[A]): Int = foldRight(list, 0)((_, acc) => acc + 1)

  def reverse[A](list: List[A]): List[A] = foldLeft(list, Nil: List[A])((acc, x) => ::(x, acc))

  def concat[A](list: List[List[A]]) = foldRight(list, Nil: List[A])(append)
  // def concat[A](list: List[List[A]]) = foldLeft(list, Nil: List[A])(append)

  def map[A, B](list: List[A])(f: A => B) = foldRight(list, Nil: List[B])((x, acc) => ::(f(x), acc))

  def range(start: Int, end: Int): List[Int] = if (start >= end) Nil else ::(start, range(start + 1, end))
  def mapWithIndex[A, B](list: List[A])(f: (A, Int) => B) = zipWith(list, range(0, length(list)))(f)

  def filter[A](list: List[A])(f: A => Boolean): List[A] = foldRight(list, Nil: List[A])((x, acc) => if (f(x)) ::(x, acc) else acc)
  // def filter[A](list: List[A])(f: A => Boolean): List[A] = flatMap(list)(x => if (f(x)) List(x) else Nil)

  def flatMap[A, B](list: List[A])(f: A => List[B]) = concat(map(list)(f))
  def concatMap[A, B](list: List[A])(f: A => List[B]) = flatMap(list)(f)

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) | (_, Nil) => Nil
    case (h1 :: t1, h2 :: t2) => ::(f(h1, h2), zipWith(t1, t2)(f))
  }

  def drop[A](list: List[A], n: Int): List[A] =
    if (n <= 0) list
    else list match {
      case Nil => Nil
      case l @ _ :: xs => if (n == 0) l else drop(xs, n - 1)
    }

  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case x :: xs if f(x) => dropWhile(xs, f)
    case _ => list
  }

  /**
  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case x :: xs => ::(x, append(xs, l2))
  }
  */

  def append[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(::(_, _))

  def appendElem[A](list: List[A], elem: A): List[A] = append(list, ::(elem, Nil))

  def prepend[A](list: List[A], elem: A): List[A] = ::(elem, list)

  def elemAtIndex[A](list: List[A], index: Int): A = list match {
    case Nil => sys.error("elem at index of Nil")
    case x :: xs => if (index == 0) x else elemAtIndex(xs, index - 1)
  }

  def setElem[A](list: List[A], index: Int, elem: A): List[A] = list match {
    case Nil => Nil
    case x :: xs => if (index == 0) ::(elem, xs) else ::(x, setElem(xs, index - 1, elem))
  }

  @annotation.tailrec
  def startsWith[A](list: List[A], prefix: List[A]): Boolean = (list, prefix) match {
    case (_, Nil) => true
    case (h1 :: t1, h2 :: t2) if (h1 == h2) => startsWith(t1, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](list: List[A], sub: List[A]): Boolean = list match {
    case Nil => sub == Nil
    case x :: xs => startsWith(list, sub) || hasSubsequence(xs, sub)
  }
}
