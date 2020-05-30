package fpinscala.datastructures.rbtrees

object RBTree {

  sealed trait Color
  case object Red extends Color
  case object Black extends Color

  sealed abstract class Tree {
    def color: Color
  }

  case class Node(color: Color, left: Tree, value: Int, right: Tree) extends Tree {
    override def toString = "( " + left.toString + " " + (if (color == Red) "R " else "B ") + value.toString + " " + right.toString + " )"
  }

  case object End extends Tree {
    override def toString = "."
    override val color: Color = Black
  }

  def insert(v: Int, t: Tree): Tree = {
    def ins(s: Tree): Tree = s match {
      case End => Node(Red, End, v, End)
      case node @ Node(_, left, value, right) =>
        if (v < value) balance(node.copy(left = ins(left)))
        else if (v > value) balance(node.copy(right = ins(right)))
        else node
    }

    ins(t) match {
      case node@Node(Red, l, v, r) => Node(Black, l, v, r)
      case node => node
    }

  }

  def balance(ggParent: Node): Tree = ggParent match {
    case Node(Black, gParent@Node(Red, parent@Node(Red, ggChild1, _, ggChild2), _, gChild), _, child) =>
      gParent.copy(color = Red,
      left = parent.copy(color = Black),
      right = ggParent.copy(color = Black, left = gChild))
    case Node(Black, gParent@Node(Red, gChild, _, parent@Node(Red, ggChild1, _, ggChild2)), _, child) =>
      parent.copy(color = Red,
      left = gParent.copy(color = Black, right = ggChild1),
      right = ggParent.copy(color = Black, left = ggChild2))
    case Node(Black, child, _, gParent@Node(Red, gChild, _, parent@Node(Red, ggChild1, _, ggChild2))) =>
      gParent.copy(color = Red,
      left = ggParent.copy(color = Black, right = gChild),
      right = parent.copy(color = Black))
    case Node(Black, child, _, gParent@Node(Red, parent@Node(Red, ggChild1, _, ggChild2), _, gChild)) =>
      parent.copy(color = Red,
      left = ggParent.copy(color = Black, right = ggChild1),
      right = gParent.copy(color = Black, left = ggChild2))
    case _ => ggParent
  }

  // not correct, maybe not balanced
  /*
  def remove(v: Int, t: Tree): Tree = {
    def rm(v: Int, s: Tree): Tree = s match {
      case End => End
      case node @ Node(_, left, value, right) =>
        if (v < value) node.copy(left = rm(v, left))
        else if (v > value) node.copy(right = rm(v, right))
        else (left, right) match {
          case (End, End) => End
          // just need to put left's value on node, then remove the duplicated value
          // values move forward, don't need to change nodes' color
          case (lNode@Node(_, gChild, lVal, _), _) => node.copy(value = lVal, left = rm(lVal, left))
          case (_,  rNode@Node(_, gChild, rVal, _)) => node.copy(value = rVal, right = rm(rVal, right))
        }
    }

    rm(v, t)
  }
  */

  def traverse(t: Tree): List[Int] = t match {
    case End => Nil
    case Node(_, left, value, right) => traverse(left) ::: value :: traverse(right)
  }

  def remove(v: Int, t: Tree): Tree = {
    traverse(t).toList.foldLeft(end)((tree, elem) => if (elem == v) tree else insert(elem, tree))
  }

  def end: Tree = End

}
