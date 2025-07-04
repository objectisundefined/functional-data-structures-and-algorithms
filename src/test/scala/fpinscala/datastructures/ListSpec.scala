package fpinscala.datastructures.lists

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class ListSpec extends AnyWordSpec with Matchers {
  import fpinscala.datastructures.lists.List._
  
  "List" should {
    "return empty List" in {
      List()
    }

    "return none empty List" in {
      List(1, 2, 3)
    }
  }

  "head" should {
    "throw Exception on empty list" in {
      intercept[java.lang.RuntimeException] {
        head(Nil)
      }
    }
    "return all but the first element of a list" in {
      head(List(1, 2, 3)) shouldEqual 1
    }
  }

  "tail" should {
    "throw Exception on empty list" in {
      intercept[java.lang.RuntimeException] {
        tail(Nil)
      }
    }
    "return all but the first element of a list" in {
      tail(List(1, 2, 3)) shouldEqual List(2, 3)
    }
  }

  "init" should {
    "throw exception on empty list" in {
      intercept[Exception] {
        init(Nil)
      }
    }
    "return all but the last element" in {
      init(List(1)) shouldEqual Nil
      init(List(1, 2, 3, 4)) shouldEqual List(1, 2, 3)
    }
  }

   "foldRight" should {
    "build sum with foldRight" in {
      def sum(l: List[Int]): Int = foldRight(l, 0)(_ + _)

      sum(List(1, 2, 3)) shouldEqual 6
      sum(Nil) shouldEqual 0
    }
    "build product with foldRight" in {
      def product(l: List[Double]): Double = foldRight(l, 1.0)(_ * _)

      product(List(1, 2, 3, 4)) shouldEqual 24.0
      product(Nil) shouldEqual 1.0
    }
    "build subtract with foldRight" in {
      def subtract(l: List[Int]): Int = foldRight(l, 0)(_ - _)

      subtract(List(1, 2, 3, 4, 5)) shouldEqual (5 - (4 - (3 - (2 - (1 - 0)))))
      subtract(Nil) shouldEqual 0
    }
  }

  "length" should {
    // scalatest length use case: "abc" should have length 3
    "return 0 on empty list" in {
      List.length(Nil) shouldEqual 0
    }
    "return the length of the given list" in {
      List.length(List(1, 2, 3)) shouldEqual 3
    }
  }

  "foldLeft" should {
    "build sum with foldLeft" in {
      def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

      sum(List(1, 2, 3)) shouldEqual 6
      sum(Nil) shouldEqual 0
    }
    "build product with foldLeft" in {
      def product(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

      product(List(1, 2, 3, 4)) shouldEqual 24.0
      product(Nil) shouldEqual 1.0
    }
    "build subtract with foldLeft" in {
      def subtract(l: List[Int]): Int = foldLeft(l, 0)(_ - _)

      subtract(List(1, 2, 3, 4, 5)) shouldEqual (0 - 1 - 2 - 3 - 4 - 5)
      subtract(Nil) shouldEqual 0
    }
  }

  "reverse" should {
    "return Nil on empty list" in {
      reverse(Nil) shouldEqual Nil
    }
    "work fine with non empty list" in {
      reverse(List("A", "B")) shouldEqual List("B", "A")
    }
  }

  "concat" should {
    "return Nil on empty list" in {
      concat(Nil) shouldEqual Nil
      concat(List(Nil)) shouldEqual Nil
    }
    "flatten the give list" in {
      concat(List(List(1, 2), List("A", "B"), List('a', 'b'))) shouldEqual List(1, 2, "A", "B", 'a', 'b')
    }
  }

  "map" should {
    "return Nil on empty list" in {
      map(Nil: List[Int])(_ * 2) shouldEqual Nil
    }
    "return a list that contains the results of appling the function on every item" in {
      map(List[Int](1, 2, 3, 4))(_ * 2) shouldEqual List(2, 4, 6, 8)
    }
  }

  "range" should {
    "return Nil if start is large than or equal to end" in {
      range(0, 0) shouldEqual Nil
      range(1, 0) shouldEqual Nil
    }
    "return [start, end) if start is less than end" in {
      range(0, 5) shouldEqual List(0, 1, 2, 3, 4)
    }
  }

  "mapWithIndex" should {
    "return a list that contains the results of appling the function on every item and it's index" in {
      mapWithIndex(List("A", "B", "C", "D"))((s, i) => s"$s$i") shouldEqual List("A0", "B1", "C2", "D3")
    }
  }

  "filter" should {
    "return Nil on empty list" in {
      filter(Nil: List[Int])(_ % 2 == 0) shouldEqual Nil
    }
    "return a list containing all elements that confirm to f" in {
      filter(List(1, 2, 3, 4))(_ % 2 == 0) shouldEqual List(2, 4)
    }
  }

  "zipWith" should {
    "work fine with lists of same length" in {
      zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldEqual List(5, 7, 9)
    }
    "work fine with lists of different length" in {
      zipWith(List(1, 2, 3), List("A", "B", "C", "D"))((n, s) => s"$n$s") shouldEqual List("1A", "2B", "3C")
    }
  }

  "drop" should {
    "return the original list if n is less or equal than 0" in {
      drop(List(1, 2, 3), 0) shouldEqual List(1, 2, 3)
      drop(List(1, 2, 3), -2) shouldEqual List(1, 2, 3)
    }
    "return Nil on empty list" in {
      drop(Nil, 10) shouldEqual Nil
    }
    "return all but the first n elements" in {
      drop(List(1, 2, 3, 4), 2) shouldEqual List(3, 4)
    }
  }

  "dropWhile" should {
    "drop the element until predicate get true" in {
      dropWhile[Int](List(1, 2, 3, 4), _ % 2 != 0) shouldEqual List(2, 3, 4)
    }
    "return Nil on empty list" in {
      dropWhile[Int](Nil, _ => true) shouldEqual Nil
    }
  }

  "append" should {
    "work fine with empty list" in {
      append(Nil, Nil) shouldEqual Nil
      append(Nil, List(1, 2)) shouldEqual List(1, 2)
      append(List(1, 2), Nil) shouldEqual List(1, 2)
    }
    "work fine with non empty list" in {
      append(List(1, 2), List(3, 4)) shouldEqual List(1, 2, 3, 4)
    }
  }

  "appendElem" should {
    "works" in {
      assert(appendElem(List(1, 2, 3), 4) == List(1, 2, 3, 4))
    }
  }

  "prepend" should {
    "works" in {
      assert(prepend(List(1, 2, 3), 0) == List(0, 1, 2, 3))
    }
  }

  "elemAtIndex" should {
    "works" in {
      assert(elemAtIndex(List(1, 2, 3), 2) == 3)
    }
  }

  "setElem" should {
    "works" in {
      assert(setElem(List(1, 2, 3), 2, 4) == List(1, 2, 4))
    }
  }

  "hasSubsequence" should {
    "return true if sub sequence is empty" in {
      hasSubsequence(Nil, Nil) shouldEqual true
    }
    "return true if xs has the sub sequence" in {
      hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3)) shouldEqual true
      hasSubsequence(List(1, 1, 2, 3, 4), List(1, 2, 3, 4)) shouldEqual true
    }
    "return false if xs does not have the sub sequence" in {
      hasSubsequence(List(1, 2, 3, 4), List(1, 4, 3)) shouldEqual false
      hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4, 5)) shouldEqual false
    }
  }

}
