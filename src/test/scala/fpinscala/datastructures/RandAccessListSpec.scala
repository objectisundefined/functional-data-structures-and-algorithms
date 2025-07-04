package fpinscala.datastructures.randomaccesslists

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class RandAccessListSpec extends AnyWordSpec with Matchers {
  import fpinscala.datastructures.randomaccesslists.RandAccessList._

  "binary increment" should {
    "works" in {
      increment(Nil) shouldEqual List(1)
      increment(List(1)) shouldEqual List(0, 1)
      increment(List(0, 1)) shouldEqual List(1, 1)
      increment(List(1, 1)) shouldEqual List(0, 0, 1)
    }
  }

  "binary decrement" should {
    "works" in {
      decrement(List(1)) shouldEqual Nil
      decrement(List(0, 1)) shouldEqual List(1)
      decrement(List(1, 1)) shouldEqual List(0, 1)
      decrement(List(0, 0, 1)) shouldEqual List(1, 1)
    }
  }

  "binary add" should {
    "works" in {
      add(Nil, Nil) shouldEqual Nil
      add(List(1), Nil) shouldEqual List(1)
      add(Nil, List(1)) shouldEqual List(1)
      add(List(1), List(1)) shouldEqual List(0, 1)
    }
  }

  "lookup" should {
    "works" in {
      val range = 0 to 8
      def f = (x: Int) => x
      val tree = range.toList.foldRight(Nil: List[Tree])((x, acc) => cons(acc, f(x)))

      for (i <- range) {
        lookup(i, tree) shouldEqual f(i)
      }
    }
  }

  "setVal" should {
    "works" in {
      val range = 0 to 8
      val tree = range.toList.foldRight(Nil: List[Tree])((a, b) => cons(b, a))
      def f = (x: Int) => x * 10

      for (i <- range) {
        lookup(i, setVal(i, f(i), tree)) shouldEqual f(i)
      }
    }
  }
}
