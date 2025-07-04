package fpinscala.datastructures.rbtrees

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class RBTreeSpec extends AnyWordSpec with Matchers {
  import fpinscala.datastructures.rbtrees.RBTree._

  def height[A](t: Tree[A]) = {
    val f = (t: Tree[A]) => t match {
      case Node(Black, _, _, _) => List[Int](1)
      case _ => List[Int](0)
    }

    val g = (a: List[Int], b: List[Int], c: List[Int]) => {
      b.flatMap((x: Int) => a.map((z: Int) => x + z)) ::: c.flatMap((y: Int) => a.map((z: Int) => y + z))
    }

    traverse(t)(f)(g)
  }

  def balanced[A](t: Tree[A]) = height(t) match {
    case Nil => true
    case x :: xs => xs.foldLeft(true) { (_, a) => a == x }
  }

  def fromList[A: Ordering](xs: List[A]): Tree[A] = xs.foldLeft(end.asInstanceOf[Tree[A]])((t: Tree[A], a: A) => insert[A](a, t))

  val root: Tree[Int] = fromList((1 to 10).toList)

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

  val words = fromList(('a' to 'z').toList)

  "red black tree" should {

    "support generic" in {
      toList(words) shouldEqual ('a' to 'z').toList
      balanced(words) shouldEqual true
      toList(remove('a', words)) shouldEqual ('a' to 'z').toList.filter(a => a != 'a')
      balanced(remove('a', words)) shouldEqual true
    }

  }

  "red black tree stringify" should {
    "work" in {
      root.toString() shouldEqual "Node Black 4 (Node Black 2 (Node Black 1 Nil Nil) (Node Black 3 Nil Nil)) (Node Black 6 (Node Black 5 Nil Nil) (Node Red 8 (Node Black 7 Nil Nil) (Node Black 9 Nil (Node Red 10 Nil Nil))))"
      remove(7, root).toString() shouldEqual "Node Black 4 (Node Black 2 (Node Black 1 Nil Nil) (Node Black 3 Nil Nil)) (Node Black 6 (Node Black 5 Nil Nil) (Node Red 9 (Node Black 8 Nil Nil) (Node Black 10 Nil Nil)))"

      val chars = fromList((1 to 10).toList.map((i: Int) => ('a'.toInt + i - 1).toChar))

      chars.toString() shouldEqual "Node Black d (Node Black b (Node Black a Nil Nil) (Node Black c Nil Nil)) (Node Black f (Node Black e Nil Nil) (Node Red h (Node Black g Nil Nil) (Node Black i Nil (Node Red j Nil Nil))))"
      remove('g', chars).toString() shouldEqual "Node Black d (Node Black b (Node Black a Nil Nil) (Node Black c Nil Nil)) (Node Black f (Node Black e Nil Nil) (Node Red i (Node Black h Nil Nil) (Node Black j Nil Nil)))"
    }
  }

  "red black tree map" should {
    "work" in {
      val chars = fromList((1 to 10).toList.map((i: Int) => ('a'.toInt + i - 1).toChar))

      val f = (c: Char) => c.toInt - 'a'.toInt + 1

      map(chars)(f) shouldEqual root
      map(remove('g', chars))(f) shouldEqual remove(f('g'), root)
    }
  }

}
