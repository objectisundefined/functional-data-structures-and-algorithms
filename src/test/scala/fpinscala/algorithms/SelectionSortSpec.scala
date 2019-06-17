package fpinscala.algorithms.sorting

import org.scalatest.{ WordSpec, Matchers }

class SelectionSortSpec extends WordSpec with Matchers {
  import fpinscala.algorithms.sorting.SelectionSort._

  "selectionSort(list)" should {
    "works when list is empty" in {
      val l: List[Integer] = List()
      val r = selectionSort(l)

      r shouldEqual Nil
    }

    "works when list has only one elem" in {
      val l: List[Integer] = List(1)
      val r = selectionSort(l)

      r shouldEqual List(1)
    }

    "works when list has two or more int elems" in {
      val l: List[Integer] = List(1, 3, 5, 2, 6)
      val r = selectionSort(l)

      r shouldEqual List(1, 2, 3, 5, 6)
    }

    "works when list has two or more string elems" in {
      val l: List[String] = List("a", "c", "e", "b", "d")
      val r = selectionSort(l)

      r shouldEqual List("a", "b", "c", "d", "e")
    }

  }

}
