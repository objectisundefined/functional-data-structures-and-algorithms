package fpinscala.datastructures.queues

object Fifo {
  // Generic Queue implementation with type parameter
  case class Fifo[A](out: List[A], in: List[A]) {
    def isEmpty: Boolean = out.isEmpty && in.isEmpty
    def size: Int = out.length + in.length
  }

  // Companion object with factory methods and operations
  object Fifo {
    def empty[A]: Fifo[A] = Fifo(Nil, Nil)
    
    def apply[A](elements: A*): Fifo[A] = 
      elements.foldLeft(empty[A])((queue, elem) => push(elem, queue))
  }

  // Push operation with proper functional style
  def push[A](element: A, queue: Fifo[A]): Fifo[A] = {
    val newIn = element :: queue.in
    queue.out match {
      case Nil => Fifo(newIn.reverse, Nil)
      case _ => queue.copy(in = newIn)
    }
  }

  // Pop operation returning Option for safe error handling
  def pop[A](queue: Fifo[A]): Option[(A, Fifo[A])] = queue.out match {
    case Nil if queue.in.isEmpty => None
    case Nil => 
      queue.in.reverse match {
        case x :: xs => Some((x, Fifo(xs, Nil)))
        case Nil => None
      }
    case x :: Nil => Some((x, Fifo(queue.in.reverse, Nil)))
    case x :: xs => Some((x, Fifo(xs, queue.in)))
  }

  // Peek operation to look at the front element without removing it
  def peek[A](queue: Fifo[A]): Option[A] = queue.out match {
    case Nil if queue.in.isEmpty => None
    case Nil => queue.in.lastOption
    case x :: _ => Some(x)
  }

  // Functional operations
  def map[A, B](queue: Fifo[A])(f: A => B): Fifo[B] = 
    Fifo(queue.out.map(f), queue.in.map(f))

  def flatMap[A, B](queue: Fifo[A])(f: A => Fifo[B]): Fifo[B] = {
    val allElements = queue.out ::: queue.in.reverse
    allElements.foldLeft(Fifo.empty[B])((acc, elem) => append(acc, f(elem)))
  }

  def filter[A](queue: Fifo[A])(predicate: A => Boolean): Fifo[A] =
    Fifo(queue.out.filter(predicate), queue.in.filter(predicate))

  def foldLeft[A, B](queue: Fifo[A], z: B)(f: (B, A) => B): B = {
    val allElements = queue.out ::: queue.in.reverse
    allElements.foldLeft(z)(f)
  }

  def foldRight[A, B](queue: Fifo[A], z: B)(f: (A, B) => B): B = {
    val allElements = queue.out ::: queue.in.reverse
    allElements.foldRight(z)(f)
  }

  // Append two queues
  def append[A](q1: Fifo[A], q2: Fifo[A]): Fifo[A] = {
    val allElements = (q1.out ::: q1.in.reverse) ::: (q2.out ::: q2.in.reverse)
    allElements.foldLeft(Fifo.empty[A])((queue, elem) => push(elem, queue))
  }

  // Convert to List
  def toList[A](queue: Fifo[A]): List[A] = queue.out ::: queue.in.reverse

  // Conversion from List
  def fromList[A](list: List[A]): Fifo[A] = 
    list.foldLeft(Fifo.empty[A])((queue, elem) => push(elem, queue))

  // Unsafe operations for compatibility (deprecated)
  @deprecated("Use pop which returns Option instead", "2.0")
  def popUnsafe[A](queue: Fifo[A]): (A, Fifo[A]) = 
    pop(queue).getOrElse(sys.error("pop on empty queue"))
}
