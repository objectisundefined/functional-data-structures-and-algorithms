package fpinscala.datastructures.queues

import org.scalatest.{ WordSpec, Matchers }

class FifoSpec extends WordSpec with Matchers {
  import fpinscala.datastructures.queues.Fifo._

  "push" should {
    "works" in {
      val q = List(1,2,3).foldLeft(Fifo(Nil, Nil))((q, e) => push(e, q))

      q.in shouldEqual List(3, 2)
      q.out shouldEqual List(1)
    }
  }

  "pop" should {
    "throw Exception on empty queue" in {
      intercept[java.lang.RuntimeException] {
        pop(Fifo(Nil, Nil))
      }
    }

    "works" in {
      val q = List(1,2,3).foldLeft(Fifo(Nil, Nil))((q, e) => push(e, q))

      val (one, q1) = pop(q)
      one shouldEqual 1

      val (two, q2) = pop(q1)
      two shouldEqual 2

      val (three, q3) = pop(q2)
      three shouldEqual 3
    }
  }

}
