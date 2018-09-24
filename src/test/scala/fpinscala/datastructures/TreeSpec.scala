package fpinscala.datastructures.trees

import org.scalatest.{ Matchers, WordSpec }

class TreeSpec extends WordSpec with Matchers {
  import fpinscala.datastructures.trees.Tree._

  "size" should {
    // scalatest size use case: "abc" should have size 3
    "return sum of leaf and branch" in {
      Tree.size(Branch(Leaf(1), Leaf(2))) shouldEqual 3
      Tree.size(Leaf(2)) shouldEqual 1
    }
  }

  "maximum" should {
    "return the max element in the tree" in {
      maximum(Branch(Leaf(1), Leaf(2))) shouldEqual 2
      maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(40), Leaf(0)))) shouldEqual 40
      maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(40))) shouldEqual 40
    }
  }

  "depth" should {
    "return the max depth of a tree" in {
      depth(Branch(Leaf(1), Leaf(2))) shouldEqual 1
      depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(40))) shouldEqual 2
    }
  }

  "map" should {
    "apply f to all element" in {
      map(Leaf(1))(_ ⇒ "A") shouldEqual Leaf("A")
      map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(40)))(_ + 1) shouldEqual Branch(Branch(Leaf(2), Leaf(3)), Leaf(41))
    }
  }

  "flip" should {
    "return leaf when flip leaf" in {
      flip(Leaf(1)) shouldEqual Leaf(1)
    }

    "return all parts reversed branch when flip branch" in {
      flip(Branch(Branch(Leaf(1), Leaf(2)), Leaf(40))) shouldEqual Branch(Leaf(40), Branch(Leaf(2), Leaf(1)))
    }
  }

  "toList" should {
    "return a list with only one elem when apply on leaf" in {
      toList(Leaf(1)) shouldEqual List(1)
    }

    "return a list contains all elems when apply on branch" in {
      toList(Branch(Branch(Leaf(1), Leaf(2)), Leaf(40))) shouldEqual List(1, 2, 40)
    }
  }

}
