package fpinscala.datastructures.heaps

object LeftistHeap {
  // Generic Tree trait with type parameter
  sealed trait Tree[+A] {
    val rank: Int
  }

  case object Leaf extends Tree[Nothing] {
    override val rank = 0
  }

  case class Node[A](rank: Int, value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

  // Companion object with factory methods
  object Tree {
    def empty[A]: Tree[A] = Leaf
    
    def single[A](value: A): Tree[A] = Node(1, value, Leaf, Leaf)
    
    def apply[A](values: A*)(implicit ord: Ordering[A]): Tree[A] =
      values.foldLeft(empty[A])((heap, value) => insert(value, heap))
  }

  // Create a node with proper leftist property
  def makeNode[A](v: A, left: Tree[A], right: Tree[A]): Node[A] = {
    if (left.rank >= right.rank) Node(left.rank + 1, v, left, right)
    else Node(right.rank + 1, v, right, left)
  }

  // Merge two heaps maintaining heap property
  def merge[A](node1: Tree[A], node2: Tree[A])(implicit ord: Ordering[A]): Tree[A] = (node1, node2) match {
    case (node, Leaf) => node
    case (Leaf, node) => node
    case (Node(_, v1, l1, r1), Node(_, v2, l2, r2)) =>
      if (ord.lteq(v1, v2)) makeNode(v1, l1, merge(r1, node2))
      else makeNode(v2, l2, merge(node1, r2))
  }

  // Insert an element
  def insert[A](value: A, heap: Tree[A])(implicit ord: Ordering[A]): Tree[A] = 
    merge(Tree.single(value), heap)

  // Get minimum element safely
  def min[A](heap: Tree[A]): Option[A] = heap match {
    case Leaf => None
    case Node(_, v, _, _) => Some(v)
  }

  // Remove minimum element safely
  def pop[A](heap: Tree[A])(implicit ord: Ordering[A]): Option[(A, Tree[A])] = heap match {
    case Leaf => None
    case Node(_, v, l, r) => Some((v, merge(l, r)))
  }

  // Check if heap is empty
  def isEmpty[A](heap: Tree[A]): Boolean = heap == Leaf

  // Get size of heap
  def size[A](heap: Tree[A]): Int = heap match {
    case Leaf => 0
    case Node(_, _, l, r) => 1 + size(l) + size(r)
  }

  // Functional operations
  def map[A, B](heap: Tree[A])(f: A => B)(implicit ord: Ordering[B]): Tree[B] = 
    heap match {
      case Leaf => Leaf
      case Node(_, v, l, r) => 
        val mappedValue = f(v)
        val mappedLeft = map(l)(f)
        val mappedRight = map(r)(f)
        merge(merge(Tree.single(mappedValue), mappedLeft), mappedRight)
    }

  def filter[A](heap: Tree[A])(predicate: A => Boolean)(implicit ord: Ordering[A]): Tree[A] = {
    def filterAcc(h: Tree[A], acc: Tree[A]): Tree[A] = h match {
      case Leaf => acc
      case Node(_, v, l, r) =>
        val newAcc = if (predicate(v)) insert(v, acc) else acc
        filterAcc(merge(l, r), newAcc)
    }
    filterAcc(heap, Tree.empty[A])
  }

  def foldLeft[A, B](heap: Tree[A], z: B)(f: (B, A) => B)(implicit ord: Ordering[A]): B = {
    def foldAcc(h: Tree[A], acc: B): B = h match {
      case Leaf => acc
      case Node(_, v, l, r) => foldAcc(merge(l, r), f(acc, v))
    }
    foldAcc(heap, z)
  }

  def foldRight[A, B](heap: Tree[A], z: B)(f: (A, B) => B)(implicit ord: Ordering[A]): B = {
    val elements = toList(heap)
    elements.foldRight(z)(f)
  }

  // Convert heap to sorted list
  def toList[A](heap: Tree[A])(implicit ord: Ordering[A]): List[A] = {
    def toListAcc(h: Tree[A], acc: List[A]): List[A] = h match {
      case Leaf => acc
      case _ =>
        pop(h) match {
          case None => acc
          case Some((v, newHeap)) => v :: toListAcc(newHeap, acc)
        }
    }
    toListAcc(heap, Nil)
  }

  // Create heap from list
  def fromList[A](list: List[A])(implicit ord: Ordering[A]): Tree[A] =
    list.foldLeft(Tree.empty[A])((heap, value) => insert(value, heap))

  // Merge multiple heaps
  def mergeAll[A](heaps: List[Tree[A]])(implicit ord: Ordering[A]): Tree[A] =
    heaps.foldLeft(Tree.empty[A])(merge)

  // Take n smallest elements
  def takeMin[A](n: Int, heap: Tree[A])(implicit ord: Ordering[A]): List[A] = {
    def takeAcc(remaining: Int, h: Tree[A], acc: List[A]): List[A] =
      if (remaining <= 0) acc.reverse
      else pop(h) match {
        case None => acc.reverse
        case Some((v, newHeap)) => takeAcc(remaining - 1, newHeap, v :: acc)
      }
    takeAcc(n, heap, Nil)
  }

  // Unsafe operations for compatibility (deprecated)
  @deprecated("Use min which returns Option instead", "2.0")
  def minUnsafe[A](heap: Tree[A]): A = 
    min(heap).getOrElse(sys.error("min on empty heap"))

  @deprecated("Use pop which returns Option instead", "2.0")
  def popUnsafe[A](heap: Tree[A])(implicit ord: Ordering[A]): (A, Tree[A]) = 
    pop(heap).getOrElse(sys.error("pop on empty heap"))
}
