package fpinscala.datastructures.heaps

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class HeapSpec extends AnyWordSpec with Matchers {
  import fpinscala.datastructures.heaps.LeftistHeap._

  val emptyH: Tree[Int] = Leaf
  val toHeap = (l: List[Int]) => l.foldLeft(emptyH)((a, b) => insert(b, a))

  "intert" should {
    "works" in {
      toHeap((1 to 8).toList)
    }
  }

  "merge" should {
    "works" in {
      merge(toHeap(List(1, 2)), toHeap(List(3, 4))) shouldEqual toHeap(List(1, 2, 3, 4))
    }
  }

  "min" should {
    "return None on empty heap" in  {
      min(emptyH) shouldEqual None
    }
    "works" in {
      min(toHeap(List(1, 2, 3, 4, 5))) shouldEqual Some(1)
    }
  }

  "pop" should {
    "return None on empty heap" in  {
      pop(emptyH) shouldEqual None
    }
    "works" in {
      val h = toHeap(List(1, 2))

      val (one, h1) = popUnsafe(h)
      one shouldEqual 1

      val (two, h2) = popUnsafe(h1)
      two shouldEqual 2
    }
  }

  "toList" should {
    "works" in {
      toList(emptyH) shouldEqual List[Int]()
      toList(toHeap(List(1, 2, 3, 4))) shouldEqual List(1, 2, 3, 4)
    }
  }
}
