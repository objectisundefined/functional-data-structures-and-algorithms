package fpinscala.datastructures.randomaccesslists

object RandAccessList {
  def increment(numList: List[Int]): List[Int] = numList match {
    case Nil => List(1)
    case 0 :: xs => 1 :: xs
    case 1 :: xs => 0 :: increment(xs)
    case _ => sys.error("Not a binary number")
  }

  def add(one: List[Int], two: List[Int]): List[Int] = (one, two) match {
    case (Nil, Nil) => Nil
    case (xs, Nil) => xs
    case (Nil, xs) => xs
    case (x :: xs, 0 :: ys) => x :: add(xs, ys)
    case (0 :: xs, y :: ys) => y :: add(xs, ys)
    case (1 :: xs, 1 :: ys) => 0 :: increment(add(xs, ys))
    case _ => sys.error("Not a binary number")
  }

  def decrement(numList: List[Int]): List[Int] = numList match {
    case 1 :: Nil => Nil
    case 1 :: xs => 0 :: xs
    case 0 :: xs => 1 :: decrement(xs)
    case _ => sys.error("Not a binary number")
  }

  sealed abstract class Tree {
    def size: Int
  }

  case class Leaf(n: Int) extends Tree {
    override def size = 1
  }

  case object Zero extends Tree {
    override def size = 0
  }

  case class One(t: Tree) extends Tree {
    override def size = t.size
  }

  case class Node(sz: Int, left: Tree, right: Tree) extends Tree {
    override def size = sz
  }

  def link(t1: Tree, t2: Tree) = Node(t1.size + t2.size, t1, t2)

  def addTreeToList(t1: Tree, listOfTrees: List[Tree]): List[Tree] = listOfTrees match {
    case Nil => List(One(t1))
    case Zero :: ts => One(t1) :: ts
    case One(t2) :: ts => Zero :: addTreeToList(link(t1, t2), ts)
    case _ => sys.error("how did it reach here?")
  }

  def cons(treeList: List[Tree], x: Int) =
    addTreeToList(Leaf(x), treeList)

  def searchTree(i: Int, tree: Tree): Int = (i, tree) match {
    case (0, Leaf(x)) => x
    case (i, Node(sz, t1, t2)) if i < (sz / 2) => searchTree(i, t1)
    case (i, Node(sz, t1, t2)) => searchTree(i - sz / 2, t2)
    case _ => sys.error("Invalid tree structure or index")
  }

  def lookup(i: Int, tree: List[Tree]): Int = tree match {
    case Zero :: ts => lookup(i, ts)
    case One(t) :: ts if i < t.size => searchTree(i, t)
    case One(t) :: ts if i >= t.size => lookup(i - t.size, ts)
    case _ => sys.error("Invalid tree list structure or index out of bounds")
  }

  def removeTree(tree: List[Tree]): (Tree, List[Tree]) = tree match {
    case One(t) :: Nil => (t, Nil)
    case One(t) :: ts => (t, Zero :: ts)
    case (Zero :: ts) => {
      val (Node(_, t1, t2), tss) = removeTree(ts)
      (t1, One(t2) :: tss)
    }
    case _ => sys.error("how did it reach here?")
  }

  def head(treeList: List[Tree]) = removeTree(treeList) match {
    case (Leaf(x), _) => x
    case _ => sys.error("how did it reach here?")
  }

  def tail(treeList: List[Tree]) = removeTree(treeList) match {
    case (_, ts) => ts
  }

  def setValInTree(i: Int, newval: Int, tree: Tree): Tree = (i, tree) match {
    case (0, Leaf(x)) => Leaf(newval)
    case (_, Node(sz, t1, t2)) if (i < sz / 2) => Node(sz, setValInTree(i, newval, t1), t2)
    case (_, Node(sz, t1, t2)) => Node(sz, t1, setValInTree(i - sz / 2, newval, t2))
    case _ => sys.error("Invalid tree structure or index")
  }

  def setVal(i: Int, newval: Int, treeList: List[Tree]): List[Tree] = treeList match {
    case Zero :: ts => Zero :: setVal(i, newval, ts)
    case One(t) :: ts if i < t.size => One(setValInTree(i, newval, t)) :: ts
    case One(t) :: ts => One(t) :: setVal(i - t.size, newval, ts)
    case _ => sys.error("Invalid tree list structure or index out of bounds")
  }

}
