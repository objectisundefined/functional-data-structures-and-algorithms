package fpinscala.algorithms.sorting

import org.scalatest.{ WordSpec, Matchers }

class MergeSortSpec extends WordSpec with Matchers {
  import fpinscala.algorithms.sorting.MergeSort._

  "merge(l1, l2)" should {
    "works" in {
      merge(List[Integer](), List[Integer]()) shouldEqual Nil
      merge(List[Integer](1), List[Integer]()) shouldEqual List(1)
      merge(List[Integer](), List[Integer](1)) shouldEqual List(1)
      merge(List[Integer](2, 3, 5), List[Integer](1, 4)) shouldEqual List(1, 2, 3, 4, 5)
    }

  }

  "split(list)" should {
    "works" in {
      split(List[Integer]()) shouldEqual (Nil, Nil)
      split(List[Integer](1)) shouldEqual (List(1), Nil)
      split(List[Integer](1, 2, 3, 4, 5)) shouldEqual (List(1, 3, 5), List(2, 4))
    }

  }

  "mergeSort(list)" should {
    "works when list is empty" in {
      val l: List[Integer] = List()
      val r = mergeSort(l)

      r shouldEqual Nil
    }

    "works when list has only one elem" in {
      val l: List[Integer] = List(1)
      val r = mergeSort(l)

      r shouldEqual List(1)
    }

    "works when list has two or more int elems" in {
      val l: List[Integer] = List(1, 3, 5, 2, 6)
      val r = mergeSort(l)

      r shouldEqual List(1, 2, 3, 5, 6)
    }

    "works when list has two or more string elems" in {
      val l: List[String] = List("a", "c", "e", "b", "d")
      val r = mergeSort(l)

      r shouldEqual List("a", "b", "c", "d", "e")
    }

  }

}
