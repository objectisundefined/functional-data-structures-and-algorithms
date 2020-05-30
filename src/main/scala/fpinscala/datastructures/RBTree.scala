package fpinscala.datastructures.rbtrees

object RBTree {

  sealed trait Color
  case object Red extends Color
  case object Black extends Color

  sealed abstract class Tree {
    def color: Color
  }

  case class Node(color: Color, value: Int, left: Tree, right: Tree) extends Tree {
    override def toString = "( " + left.toString + " " + (if (color == Red) "R " else "B ") + value.toString + " " + right.toString + " )"
  }

  case object End extends Tree {
    override def toString = "."
    override val color: Color = Black
  }

  def blacken(t: Tree): Tree = t match {
    case node@Node(Red, _, _, _) => node.copy(color = Black)
    case _ => t
  }

  def insert(x: Int, t: Tree): Tree = {
    def ins(s: Tree): Tree = s match {
      case End => Node(Red, x, End, End)
      case node @ Node(_, v, l, r) =>
        if (x < v) balance(node.copy(left = ins(l)))
        else if (x > v) balance(node.copy(right = ins(r)))
        else node
    }

    blacken(ins(t))
  }

  private[this] def balance(ggParent: Node): Tree = ggParent match {
    case Node(Black, _, gParent@Node(Red, _, parent@Node(Red, _, ggChild1, ggChild2), gChild), child) =>
      gParent.copy(color = Red,
      left = parent.copy(color = Black),
      right = ggParent.copy(color = Black, left = gChild))
    case Node(Black, _, gParent@Node(Red, _, gChild, parent@Node(Red, _, ggChild1, ggChild2)), child) =>
      parent.copy(color = Red,
      left = gParent.copy(color = Black, right = ggChild1),
      right = ggParent.copy(color = Black, left = ggChild2))
    case Node(Black, _, child, gParent@Node(Red, _, gChild, parent@Node(Red, _, ggChild1, ggChild2))) =>
      gParent.copy(color = Red,
      left = ggParent.copy(color = Black, right = gChild),
      right = parent.copy(color = Black))
    case Node(Black, _, child, gParent@Node(Red, _, parent@Node(Red, _, ggChild1, ggChild2), gChild)) =>
      parent.copy(color = Red,
      left = ggParent.copy(color = Black, right = ggChild1),
      right = gParent.copy(color = Black, left = ggChild2))
    case _ => ggParent
  }

  def traverse[A](t: Tree)(f: Tree => A)(g: (A, A, A) => A): A = t match {
    case End => f(End)
    case node@Node(_, _, l, r) => g(f(node), traverse(l)(f)(g), traverse(r)(f)(g))
  }

  def toList(t: Tree): List[Int] = {
    val f = (t: Tree) => t match {
      case End => List[Int]()
      case Node(_, x, _, _) => List[Int](x)
    }

    val g = (a: List[Int], b: List[Int], c: List[Int]) => b ::: a ::: c

    traverse(t)(f)(g)
  }

  // delete by re creating
  /*
  def remove(v: Int, t: Tree): Tree = {
    toList(t).foldLeft(end)((tree, elem) => if (elem == v) tree else insert(elem, tree))
  }
  */

  def isBlack(t: Tree): Boolean = t match {
    case Node(Red, _, _, _) => false
    case _ => true
  }

  private[this] def balL(c: Color, y: Int, lf: (Tree, Boolean), r: Tree): (Tree, Boolean) = {
    val (l, f)  = lf

    if (f) (Node(c, y, l, r), true) else balL_(c, y, l, r)
  }

  private[this] def balR(c: Color, y: Int, l: Tree, rf: (Tree, Boolean)): (Tree, Boolean) = {
    val (r, f) = rf

    if (f) (Node(c, y, l, r), true) else balR_(c, y, l, r)
  }

  private[this] def balL_(c1: Color, p: Int, n: Tree, r: Tree): (Tree, Boolean)  = {
    val Node(c2, s, sl, sr) = r

    if (c2 == Red) balL(Black, s, balL_(Red, p, n, sl), sr)
    else if (isBlack(sl) && isBlack(sr)) (Node(Black, p, n, Node(Red, s, sl, sr)), c1 == Red)
    else if (!isBlack(sr)) (Node(c1, s, Node(Black, p, n, sl), blacken(sr)), true)
    else {
      val Node(Red, x, sll, slr) = sl
      balL_(c1, p, n, Node(Black, x, sll, Node(Red, s, slr, sr)))
    }
  }

  private[this] def balR_(c1: Color, p: Int, l: Tree, n: Tree): (Tree, Boolean) = {
    val Node(c2, s, sl, sr) = l

    if (c2 == Red) balR(Black, s, sl, balR_(Red, p, sr, n))
    else if (isBlack(sl) && isBlack(sr)) (Node(Black, p, Node(Red, s, sl, sr), n), c1 == Red)
    else if (!isBlack(sl)) (Node(c1, s, blacken(sl), Node(Black, p, sr, n)), true)
    else {
      val Node(Red, x, srl, srr) = sr
      balR_(c1, p, Node(Black, x, Node(Red, s, sl, srl), srr), n)
    }
  }

  private[this] def deleteRoot(t: Tree): (Tree, Boolean) = t match {
    case Node(c, _, End, End) => (End, c == Red)
    case Node(_, _, l, End) => (blacken(l), true)
    case Node(_, _, End, r) => (blacken(r), true)
    case Node(c, _, l, r) => {
      val m = findMin(r)
      balR(c, m, l, delete_(m, r))
    }
    case _ => sys.error("deleteRoot(t: Tree): t should be Node")
  }

  private[this] def findMin(t: Tree): Int = t match {
    case Node(_, x, End, _) => x
    case Node(_, _, l, _) => findMin(l)
    case _ => sys.error("findMin(t: Tree): t should be Node")
  }

  private[this] def delete_(x: Int, t: Tree): (Tree, Boolean) = t match {
    case End => (End, true)
    case root@Node(c, y, l, r) =>
      if (x < y) balL(c, y, delete_(x, l), r)
      else if (x > y) balR(c, y, l, delete_(x, r))
      else deleteRoot(root)
  }

  def remove(x: Int, t: Tree): Tree = delete_(x, t)._1

  def end: Tree = End

}

// delete implementation detail
// https://zhuanlan.zhihu.com/p/77616103
