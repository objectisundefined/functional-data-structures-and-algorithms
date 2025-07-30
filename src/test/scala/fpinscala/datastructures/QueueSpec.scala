package fpinscala.datastructures.queues

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class QueueSpec extends AnyWordSpec with Matchers {
  import fpinscala.datastructures.queues.Fifo._

  "push" should {
    "works" in {
      val q = List(1,2,3).foldLeft(Fifo.empty[Int])((q, e) => push(e, q))

      q.in shouldEqual List(3, 2)
      q.out shouldEqual List(1)
    }
  }

  "pop" should {
    "return None on empty queue" in {
      pop(Fifo.empty[Int]) shouldEqual None
    }

    "works" in {
      val q = List(1,2,3).foldLeft(Fifo.empty[Int])((q, e) => push(e, q))

      val (one, q1) = popUnsafe(q)
      one shouldEqual 1

      val (two, q2) = popUnsafe(q1)
      two shouldEqual 2

      val (three, q3) = popUnsafe(q2)
      three shouldEqual 3
    }
  }

}
