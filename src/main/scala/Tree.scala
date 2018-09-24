package com.example.fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }
  */
  def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)

  /**
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }
  */
  def depth[A](tree: Tree[A]): Int = fold(tree)(_ => 0)(1 + _ max _)

  /**
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }
  */
  def maximum(tree: Tree[Int]): Int = fold(tree)(a => a)(_ max _)

  /**
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  /**
  buildCompleteTree

  buildTree

  flip

  equal

  flippedEqual

  preorder

  inorder

  postorder

  preorderAcc

  inorderAcc

  postorderAcc
  */
}
