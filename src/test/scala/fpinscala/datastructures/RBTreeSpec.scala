package fpinscala.datastructures.rbtrees

import org.scalatest.{ Matchers, WordSpec }

class TreeSpec extends WordSpec with Matchers {
  import fpinscala.datastructures.rbtrees.RBTree._

  val height = (t: Tree) => {
    val f = (t: Tree) => t match {
      case Node(Black, _, _, _) => List[Int](1)
      case _ => List[Int](0)
    }

    val g = (a: List[Int], b: List[Int], c: List[Int]) => {
      b.flatMap((x: Int) => a.map((z: Int) => x + z)) ::: c.flatMap((y: Int) => a.map((z: Int) => y + z))
    }

    traverse(t)(f)(g)
  }

  val balanced: Function1[Tree, Boolean] = (t: Tree) => height(t) match {
    case Nil => true
    case x :: xs => xs.foldLeft(true) { (_, a) => a == x }
  }

  val root: Tree = (1 to 10).toList.foldLeft(end)((t: Tree, a: Int) => insert(a, t))

  "red black tree" should {

    "be ordered after insert" in {
      toList(root) shouldEqual (1 to 10).toList
    }

    "be balanced after insert" in {
      balanced(root) shouldEqual true
    }

    "be ordered after remove" in {
      toList(remove(7, root)) shouldEqual (1 to 10).toList.filter(a => a != 7)
    }

    "be balanced after remove" in {
      balanced(remove(7, root)) shouldEqual true
    }
  }

}
