package fpinscala.algorithms.sorting

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class InsertionSortSpec extends AnyWordSpec with Matchers {
  import fpinscala.algorithms.sorting.InsertionSort._

  "insertionSort(list)" should {
    "works when list is empty" in {
      val l: List[Integer] = List()
      val r = insertionSort(l)

      r shouldEqual Nil
    }

    "works when list has only one elem" in {
      val l: List[Integer] = List(1)
      val r = insertionSort(l)

      r shouldEqual List(1)
    }

    "works when list has two or more int elems" in {
      val l: List[Integer] = List(1, 3, 5, 2, 6)
      val r = insertionSort(l)

      r shouldEqual List(1, 2, 3, 5, 6)
    }

    "works when list has two or more string elems" in {
      val l: List[String] = List("a", "c", "e", "b", "d")
      val r = insertionSort(l)

      r shouldEqual List("a", "b", "c", "d", "e")
    }

    "works when list has two same elems" in {
      val l: List[Integer] = List(1, 3, 5, 2, 6, 3)
      val r = insertionSort(l)

      r shouldEqual List(1, 2, 3, 3, 5, 6)
    }

  }

}
