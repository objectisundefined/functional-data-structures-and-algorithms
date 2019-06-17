package fpinscala.algorithms.sorting

import org.scalatest.{ WordSpec, Matchers }

class BubbleSortSpec extends WordSpec with Matchers {
  import fpinscala.algorithms.sorting.BubbleSort._

  "getLargest(list)" should {
    "works when list is empty" in {
      val l: List[Integer] = List()
      val (a, r) = getLargest(l)

      a shouldEqual null
      r shouldEqual Nil
    }

    "works when list has only one elem" in {
      val l: List[Integer] = List(1)
      val (a, r) = getLargest(l)

      a shouldEqual 1
      r shouldEqual Nil
    }

    "works when list has two or more int elems" in {
      val l: List[Integer] = List(1, 3, 5, 2, 6)
      val (a, r) = getLargest(l)

      a shouldEqual 6
      r shouldEqual List(1, 3, 5, 2)
    }

    "works when list has two or more string elems" in {
      val l: List[String] = List("a", "c", "e", "b", "d")
      val (a, r) = getLargest(l)

      a shouldEqual "e"
      r shouldEqual List("a", "c", "d", "b")
    }

  }

  "bubbleSort(list)" should {
    "works when list is empty" in {
      val l: List[Integer] = List()
      val r = bubbleSort(l)

      r shouldEqual Nil
    }

    "works when list has only one elem" in {
      val l: List[Integer] = List(1)
      val r = bubbleSort(l)

      r shouldEqual List(1)
    }

    "works when list has two or more int elems" in {
      val l: List[Integer] = List(1, 3, 5, 2, 6)
      val r = bubbleSort(l)

      r shouldEqual List(1, 2, 3, 5, 6)
    }

    "works when list has two or more string elems" in {
      val l: List[String] = List("a", "c", "e", "b", "d")
      val r = bubbleSort(l)

      r shouldEqual List("a", "b", "c", "d", "e")
    }

    "works when list has two same elems" in {
      val l: List[Integer] = List(1, 3, 5, 2, 6, 3)
      val r = bubbleSort(l)

      r shouldEqual List(1, 2, 3, 3, 5, 6)
    }

  }

}
