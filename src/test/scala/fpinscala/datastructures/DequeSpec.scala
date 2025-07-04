package fpinscala.datastructures.deques

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class DequeSpec extends AnyWordSpec with Matchers {
  import fpinscala.datastructures.deques.Deque._

  "deque" should {
    "works when pushFront first then popFront" in {
      val dq = Deque(0, LazyList.empty, 0, LazyList.empty)
      val dq1 = dq.pushFront(1)
      val dq2 =  dq1.pushFront(2)
      val dq3 = dq2.pushFront(3)

      val (x,p) = dq3.popFront()
      x shouldEqual 3

      val (y,p1) = p.popFront()
      y shouldEqual 2

      val (z,p2) = p1.popFront()
      z shouldEqual 1

      intercept[java.lang.RuntimeException] {
        p2.popFront()
      }
    }

    "works when pushFront first then popEnd" in {
      val dq = Deque(0, LazyList.empty, 0, LazyList.empty)
      val dq1 = dq.pushFront(1)
      val dq2 =  dq1.pushFront(2)
      val dq3 = dq2.pushFront(3)

      val (x,p) = dq3.popEnd()
      x shouldEqual 1

      val (y,p1) = p.popEnd()
      y shouldEqual 2

      val (z,p2) = p1.popEnd()
      z shouldEqual 3

      intercept[java.lang.RuntimeException] {
        p2.popEnd()
      }
    }

    "works when pushEnd first then popEnd" in {
      val dq = Deque(0, LazyList.empty, 0, LazyList.empty)
      val dq1 = dq.pushEnd(1)
      val dq2 =  dq1.pushEnd(2)
      val dq3 = dq2.pushEnd(3)

      val (x,p) = dq3.popEnd()
      x shouldEqual 3

      val (y,p1) = p.popEnd()
      y shouldEqual 2

      val (z,p2) = p1.popEnd()
      z shouldEqual 1

      intercept[java.lang.RuntimeException] {
        p2.popEnd()
      }
    }

    "works when pushEnd first then popFront" in {
      val dq = Deque(0, LazyList.empty, 0, LazyList.empty)
      val dq1 = dq.pushEnd(1)
      val dq2 =  dq1.pushEnd(2)
      val dq3 = dq2.pushEnd(3)

      val (x,p) = dq3.popFront()
      x shouldEqual 1

      val (y,p1) = p.popFront()
      y shouldEqual 2

      val (z,p2) = p1.popFront()
      z shouldEqual 3

      intercept[java.lang.RuntimeException] {
        p2.popFront()
      }
    }
  }

}
