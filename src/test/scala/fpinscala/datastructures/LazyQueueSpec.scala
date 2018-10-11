package fpinscala.datastructures.queues

import org.scalatest.{ WordSpec, Matchers }

class LazyQueueSpec extends WordSpec with Matchers {
  import fpinscala.datastructures.queues.LazyQueue._

  "LazyQueue" should {
    "works when push first then pop" in {
      val emptyQ = makeLazyQueue(Stream.empty, 0, Nil, 0)
      val q = List(10, 20, 30).foldLeft(emptyQ)((acc, x) => acc.push(x))

      val (a1, q1) = q.pop
      a1 shouldEqual 10

      val (a2, q2) = q1.pop
      a2 shouldEqual 20

      val (a3, q3) = q2.pop
      a3 shouldEqual 30
    }

    "throw Exception on empty queue" in {
      intercept[java.lang.RuntimeException] {
        val emptyQ = makeLazyQueue(Stream.empty, 0, Nil, 0)
        emptyQ.pop
      }
    }
  }

}
