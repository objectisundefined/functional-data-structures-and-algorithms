package fpinscala.algorithms.sorting

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class QuickSortSpec extends AnyWordSpec with Matchers {
  import fpinscala.algorithms.sorting.QuickSort._

  "partition(e, l, fp, sp)" should {
    "works" in {
      val l = List[Integer](1, 3, 5, 4, 2, 3)

      partition[Integer](0, l, Nil, Nil) shouldEqual (Nil, List(3, 2, 4, 5, 3, 1))
      partition[Integer](1, l, Nil, Nil) shouldEqual (Nil, List(3, 2, 4, 5, 3, 1))
      partition[Integer](2, l, Nil, Nil) shouldEqual (List(1), List(3, 2, 4, 5, 3))
      partition[Integer](3, l, Nil, Nil) shouldEqual (List(2, 1), List(3, 4, 5, 3))
      partition[Integer](4, l, Nil, Nil) shouldEqual (List(3, 2, 3, 1), List(4, 5))
      partition[Integer](5, l, Nil, Nil) shouldEqual (List(3, 2, 4, 3, 1), List(5))
      partition[Integer](6, l, Nil, Nil) shouldEqual (List(3, 2, 4, 5, 3, 1), Nil)
    }

  }

  "quickSort(list)" should {
    "works when list is empty" in {
      val l: List[Integer] = List()
      val r = quickSort(l)

      r shouldEqual Nil
    }

    "works when list has only one elem" in {
      val l: List[Integer] = List(1)
      val r = quickSort(l)

      r shouldEqual List(1)
    }

    "works when list has two or more int elems" in {
      val l: List[Integer] = List(1, 3, 5, 2, 6)
      val r = quickSort(l)

      r shouldEqual List(1, 2, 3, 5, 6)
    }

    "works when list has two or more string elems" in {
      val l: List[String] = List("a", "c", "e", "b", "d")
      val r = quickSort(l)

      r shouldEqual List("a", "b", "c", "d", "e")
    }

    "works when list has two same elems" in {
      val l: List[Integer] = List(1, 3, 5, 2, 6, 3)
      val r = quickSort(l)

      r shouldEqual List(1, 2, 3, 3, 5, 6)
    }

  }

}
