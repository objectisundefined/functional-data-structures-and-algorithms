package fpinscala.datastructures.rbtrees

object RBTree {

  sealed trait Color
  case object Red extends Color
  case object Black extends Color

  sealed trait Tree[+A] {
    def color: Color
  }

  case class Node[A](color: Color, value: A, left: Tree[A], right: Tree[A]) extends Tree[A] {

    override def toString = {
      val f = (t: Tree[A]) => t match {
        case End => t.toString
        case _ => "(" + t.toString + ")"
      }

      "Node " + (if (color == Red) "Red " else "Black ") + value.toString + " " + f(left) +  " " + f(right)
    }
  }

  case object End extends Tree[Nothing] {
    override val color: Color = Black
    override def toString = "Nil"
  }

  def blacken[A](t: Tree[A]): Tree[A] = t match {
    case node@Node(Red, _, _, _) => node.copy(color = Black)
    case _ => t
  }

  def insert[A: Ordering](x: A, t: Tree[A]): Tree[A] = {
    def ins(s: Tree[A]): Tree[A] = s match {
      case End => Node(Red, x, End, End)
      case node @ Node(_, v, l, r) =>
        // change to generic comparing
        /*
        if (x < v) balance(node.copy(left = ins(l)))
        else if (x > v) balance(node.copy(right = ins(r)))
        else node
        */

        val ord = implicitly[Ordering[A]].compare(x, v)

        if (ord < 0) balance(node.copy(left = ins(l)))
        else if (ord > 0) balance(node.copy(right = ins(r)))
        else node

    }

    blacken(ins(t))
  }

  private[this] def balance[A](ggParent: Node[A]): Tree[A] = ggParent match {
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

  def traverse[A, B](t: Tree[A])(f: Tree[A] => B)(g: (B, B, B) => B): B = t match {
    case End => f(End)
    case node@Node(_, _, l, r) => g(f(node), traverse(l)(f)(g), traverse(r)(f)(g))
  }

  def toList[A](t: Tree[A]): List[A] = {
    val f = (t: Tree[A]) => t match {
      case End => List[A]()
      case Node(_, x, _, _) => List[A](x)
    }

    val g = (a: List[A], b: List[A], c: List[A]) => b ::: a ::: c

    traverse(t)(f)(g)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case End => End
    case Node(c, x, l, r) => Node(c, f(x), map(l)(f), map(r)(f))
  }

  // delete by re creating
  /*
  def remove[A: Ordering](v: A, t: Tree[A]): Tree[A] = {
    toList(t).foldLeft(end.asInstanceOf[Tree[A]])((tree, elem) => if (elem == v) tree else insert[A](elem, tree))
  }
  */

  def isBlack[A](t: Tree[A]): Boolean = t match {
    case Node(Red, _, _, _) => false
    case _ => true
  }

  private[this] def balL[A](c: Color, y: A, lf: (Tree[A], Boolean), r: Tree[A]): (Tree[A], Boolean) = {
    val (l, f)  = lf

    if (f) (Node(c, y, l, r), true) else balL_(c, y, l, r)
  }

  private[this] def balR[A](c: Color, y: A, l: Tree[A], rf: (Tree[A], Boolean)): (Tree[A], Boolean) = {
    val (r, f) = rf

    if (f) (Node(c, y, l, r), true) else balR_(c, y, l, r)
  }

  private[this] def balL_[A](c1: Color, p: A, n: Tree[A], r: Tree[A]): (Tree[A], Boolean)  = {
    val Node(c2, s, sl, sr) = r

    if (c2 == Red) balL(Black, s, balL_(Red, p, n, sl), sr)
    else if (isBlack(sl) && isBlack(sr)) (Node(Black, p, n, Node(Red, s, sl, sr)), c1 == Red)
    else if (!isBlack(sr)) (Node(c1, s, Node(Black, p, n, sl), blacken(sr)), true)
    else {
      val Node(Red, x, sll, slr) = sl
      balL_(c1, p, n, Node(Black, x, sll, Node(Red, s, slr, sr)))
    }
  }

  private[this] def balR_[A](c1: Color, p: A, l: Tree[A], n: Tree[A]): (Tree[A], Boolean) = {
    val Node(c2, s, sl, sr) = l

    if (c2 == Red) balR(Black, s, sl, balR_(Red, p, sr, n))
    else if (isBlack(sl) && isBlack(sr)) (Node(Black, p, Node(Red, s, sl, sr), n), c1 == Red)
    else if (!isBlack(sl)) (Node(c1, s, blacken(sl), Node(Black, p, sr, n)), true)
    else {
      val Node(Red, x, srl, srr) = sr
      balR_(c1, p, Node(Black, x, Node(Red, s, sl, srl), srr), n)
    }
  }

  private[this] def deleteRoot[A: Ordering](t: Tree[A]): (Tree[A], Boolean) = t match {
    case Node(c, _, End, End) => (End, c == Red)
    case Node(_, _, l, End) => (blacken(l), true)
    case Node(_, _, End, r) => (blacken(r), true)
    case Node(c, _, l, r) => {
      val m = findMin(r)
      balR(c, m, l, delete_(m, r))
    }
    case _ => sys.error("deleteRoot[A](t: Tree[A]): t should be Node[A]")
  }

  private[this] def findMin[A](t: Tree[A]): A = t match {
    case Node(_, x, End, _) => x
    case Node(_, _, l, _) => findMin(l)
    case _ => sys.error("findMin[A](t: Tree[A]): t should be Node[A]")
  }

  private[this] def delete_[A: Ordering](x: A, t: Tree[A]): (Tree[A], Boolean) = t match {
    case End => (End, true)
    case root@Node(c, y, l, r) =>
      // change to generic comparing
      /*
      if (x < y) balL(c, y, delete_(x, l), r)
      else if (x > y) balR(c, y, l, delete_(x, r))
      else deleteRoot(root)
      */

      val ord = implicitly[Ordering[A]].compare(x, y)

      if (ord < 0) balL(c, y, delete_(x, l), r)
      else if (ord > 0) balR(c, y, l, delete_(x, r))
      else deleteRoot[A](root)
  }

  def remove[A: Ordering](x: A, t: Tree[A]): Tree[A] = delete_(x, t)._1

  def end: Tree[Nothing] = End

}

// delete implementation detail
// https://zhuanlan.zhihu.com/p/77616103
