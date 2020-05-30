package fpinscala.datastructures.rbtrees

import org.scalatest.{ Matchers, WordSpec }

class TreeSpec extends WordSpec with Matchers {
  import fpinscala.datastructures.rbtrees.RBTree._

  def height: Function1[Tree, List[Int]] = (t: Tree) => {
    t match {
      case End => List(0)
      case Node(c, l, _, r) => ((n: Int) => height(l).map((x: Int) => x + n) ::: height(r).map((x: Int) => x + n))(if (c == Red) 0 else 1)
    }
  }

  def balanced (t: Tree): Boolean = height(t) match {
    case Nil => true
    case x :: xs => xs.foldLeft(true) { (_, a) => a == x }
  }

  val root: Tree = (1 to 10).toList.foldLeft(end)((t: Tree, a: Int) => insert(a, t))

  "red black tree" should {

    "insert should be ordered" in {
      traverse(root) shouldEqual (1 to 10).toList
    }

    "insert should be balanced" in {
      balanced(root) shouldEqual true
    }

    "remove should be ordered" in {
      traverse(remove(7, root)) shouldEqual (1 to 10).toList.filter(a => a != 7)
    }

    "remove should be balanced" in {
      balanced(remove(7, root)) shouldEqual true
    }
  }

}
