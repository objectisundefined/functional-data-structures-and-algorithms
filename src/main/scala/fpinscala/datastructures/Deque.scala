package fpinscala.datastructures.deques

object Deque {

  case class Deque(outLen: Int, out: LazyList[Int], inLen: Int, in: LazyList[Int], c: Int = 2) {
    def pushFront(elem: Int): Deque = {
      adjustStreams(outLen + 1, LazyList.cons(elem, out), inLen, in, c)
    }

    def popFront(): (Int, Deque) = {
      out match {
        case LazyList() => in match {
          case LazyList() => sys.error("empty queue")
          case _ =>
            val newOut = in.reverse
            (newOut.head, Deque(inLen - 1, newOut.tail, 0, LazyList.empty, c))
        }
        case x #:: newOut => (x, adjustStreams(outLen - 1, newOut, inLen, in, c))
      }
    }

    def pushEnd(elem: Int): Deque = {
      adjustStreams(outLen, out, inLen + 1, LazyList.cons(elem, in), c)
    }

    def popEnd(): (Int, Deque) = {
      in match {
        case LazyList() => out match {
          case LazyList() => sys.error("empty queue")
          case _ =>
            val newIn = out.reverse
            (newIn.head, Deque(0, LazyList.empty, outLen - 1, newIn.tail, c))
        }
        case x #:: newIn => (x, adjustStreams(outLen, out, inLen - 1, newIn, c))
      }
    }
  }

  def adjustStreams(outLen: Int, out: LazyList[Int], inLen: Int, in: LazyList[Int], c: Int): Deque = {
    if (outLen > c * inLen + 1) {
      val newOutLen = (outLen + inLen ) / 2
      val newInLen  = outLen + inLen - newOutLen

      val newOut = out.take(newOutLen)
      val newIn = in.lazyAppendedAll(out.drop(newInLen).reverse)

      Deque(newOutLen, newOut, newInLen, newIn, c)
    } else if (inLen > c * outLen + 1) {
      val newInLen = (outLen + inLen ) / 2
      val newOutLen = outLen + inLen - newInLen

      val newIn = in.take(newInLen)
      val newOut = out.lazyAppendedAll(in.drop(newOutLen).reverse)

      Deque(newOutLen, newOut, newInLen, newIn, c)
    } else {
      Deque(outLen, out, inLen, in, c)
    }
  }

}
