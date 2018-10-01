package fpinscala.datastructures.heaps

object LeftistHeap {
  sealed trait Tree {
    val rank: Int
  }

  case object Leaf extends Tree {
    override val rank = 0
  }

  case class Node(rank: Int, value: Int, left: Tree, right: Tree) extends Tree

  def makeNode(v: Int, left: Tree, right: Tree): Node = {
    if (left.rank >= right.rank) Node(left.rank + 1, v, left, right)
    else Node(right.rank + 1, v, right, left)
  }

  def merge(node1: Tree, node2: Tree): Tree = (node1, node2) match {
    case (node, Leaf) => node
    case (Leaf, node) => node
    case (Node(_, v1, l1, r1), Node(_, v2, l2, r2)) =>
      if (v1 < v2) makeNode(v1, l1, merge(r1, node2))
      else makeNode(v2, l2, merge(node1, r2))
  }

  def insert(v: Int, h: Tree): Tree = merge(makeNode(v, Leaf, Leaf), h)

  def min (h:Tree): Int = h match {
    case Leaf => sys.error("min on leaf")
    case Node(_, v, _, _) => v
  }

  def pop(h:Tree): (Int, Tree) = h match {
    case Leaf => sys.error("pop on leaf")
    case Node(_, v, l, r) => (v, merge(l, r))
  }

  val toList: Tree => List[Int] = h => h match {
    case Leaf => List[Int]()
    case _ =>
      val (v, n) = pop(h)
      v :: toList(n)
  }

}
