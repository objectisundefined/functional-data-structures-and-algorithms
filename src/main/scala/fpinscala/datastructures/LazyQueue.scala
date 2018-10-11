package fpinscala.datastructures.queues

import scala.collection.SeqView

object LazyQueue {
  case class LazyQueue(out: Stream[Int], outLen: Int, in: List[Int], inLen: Int) {
    def push(elem: Int) = makeLazyQueue(out, outLen, elem :: in, inLen + 1)

    def pop: (Int, LazyQueue) = (out.head, makeLazyQueue(out.tail, outLen - 1, in, inLen))

    def empty = out.isEmpty && in.isEmpty
  }

  def makeLazyQueue(out: Stream[Int], outLen: Int, in: List[Int], inLen: Int): LazyQueue = {
    if (inLen <= outLen) LazyQueue(out, outLen, in, inLen)
    else {
      val newOut = copyInToOut(out, in, Stream.empty)
      LazyQueue(newOut, outLen + inLen, Nil, 0)
    }
  }

  def copyInToOut(out: Stream[Int], in: List[Int], revIn: Stream[Int]): Stream[Int] = in match {
    case Nil => Stream.empty
    case x :: xs if out.isEmpty => Stream.cons(x, revIn)
    case x :: xs => Stream.cons(out.head, copyInToOut(out.tail, in.tail, Stream.cons(x, revIn)))
  }

}
