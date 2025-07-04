package fpinscala.datastructures.queues

object LazyQueue {
  case class LazyQueue(out: LazyList[Int], outLen: Int, in: List[Int], inLen: Int) {
    def push(elem: Int) = makeLazyQueue(out, outLen, elem :: in, inLen + 1)

    def pop: (Int, LazyQueue) = (out.head, makeLazyQueue(out.tail, outLen - 1, in, inLen))

    def empty = out.isEmpty && in.isEmpty
  }

  def makeLazyQueue(out: LazyList[Int], outLen: Int, in: List[Int], inLen: Int): LazyQueue = {
    if (inLen <= outLen) LazyQueue(out, outLen, in, inLen)
    else {
      val newOut = copyInToOut(out, in, LazyList.empty)
      LazyQueue(newOut, outLen + inLen, Nil, 0)
    }
  }

  def copyInToOut(out: LazyList[Int], in: List[Int], revIn: LazyList[Int]): LazyList[Int] = in match {
    case Nil => LazyList.empty
    case x :: xs if out.isEmpty => LazyList.cons(x, revIn)
    case x :: xs => LazyList.cons(out.head, copyInToOut(out.tail, in.tail, LazyList.cons(x, revIn)))
  }

}
