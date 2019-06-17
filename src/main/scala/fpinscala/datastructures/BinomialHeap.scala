package fpinscala.datastructures.binomealheaps

object BinomealHeap {

  case class Node(rank: Int, v: Int, children: List[Node])

  def linkUp(t1: Node, t2: Node): Node =
    if (t1.v <= t2.v)
      Node(t1.rank + 1, t1.v, t2 :: t1.children)
    else
      Node(t1.rank + 1, t2.v, t1 :: t2.children)

  def insert(t: Node, rootList: List[Node]): List[Node] = rootList match {
    case Nil => List(t)
    case x :: xs if (t.rank < x.rank) => t :: rootList
    case x :: xs => insert(linkUp(t, x), xs)
  }

  def insertElem(rootList: List[Node], elem: Int): List[Node] =
    insert(Node(0, elem, Nil), rootList)

  def merge(l1: List[Node], l2: List[Node]): List[Node] = (l1, l2) match {
    case (_, Nil) => l1
    case (Nil, _) => l2
    case (x :: xs, y :: ys) if (x.rank < y.rank) => x :: merge(xs, l2)
    case (x :: xs, y :: ys) if (x.rank > y.rank) => y :: merge(l1, ys)
    case (x :: xs, y :: ys) => insert(linkUp(x, y), merge(xs, ys))
  }

  def findMin(rootList: List[Node]): Node =
    rootList min (Ordering.by((p: Node) => p.v))

  def removeMin(rootList: List[Node]): List[Node] = {
    val min = findMin(rootList)
    val rest = rootList filterNot (_ == min)
    merge(rest, min.children)
  }

  val list = List(13, 12, 24, 4, 15, 28, 21, 9, 11, 17, 32, 14, 20, 41, 30)

  val v = list.foldLeft[List[Node]](List[Node]())(insertElem)

  val min = findMin(v)

  println(min.v)

  val v1 = removeMin(v)

  val min1 = findMin(v1)

  println(min1.v)

}
