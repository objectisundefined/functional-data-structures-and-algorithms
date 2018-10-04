package fpinscala.datastructures.queues

object Fifo {
  case class Fifo(out: List[Int], in: List[Int]) {}

  def push(e: Int, queue: Fifo) = {
    val in = e :: queue.in

    queue.out match {
      case Nil => Fifo(in.reverse, Nil)
      case _ => queue.copy(in = in)
    }
  }

  def pop(queue: Fifo): (Int, Fifo) = queue.out match {
    case Nil => sys.error("pop on empty queue")
    case x :: Nil => (x, queue.copy(out = queue.in.reverse, Nil))
    case x :: xs => (x, queue.copy(out = xs))
  }

}
