package fpinscala.algorithms.sorting

object BubbleSort {
  def getLargest[T](l: List[T])(implicit ev: T => Ordered[T]): Option[(T, List[T])] = l match {
    case Nil => None
    case x :: Nil => Some((x, Nil))
    case x :: xs =>
      getLargest(xs) match {
        case None => Some((x, Nil))
        case Some((a, r)) => 
          if (x >= a) Some((x, a :: r))
          else Some((a, x :: r))
      }
  }

  def bubbleSort[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = 
    getLargest(l) match {
      case None => Nil
      case Some((largest, rest)) => bubbleSort(rest) ::: List(largest)
    }

  // Alternative implementation using fold for a more functional approach
  def bubbleSortFold[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = {
    def bubblePass(xs: List[T]): List[T] = xs match {
      case Nil | _ :: Nil => xs
      case x :: y :: tail =>
        if (x <= y) x :: bubblePass(y :: tail)
        else y :: bubblePass(x :: tail)
    }
    
    def bubbleUntilSorted(xs: List[T], n: Int): List[T] = 
      if (n <= 1) xs
      else bubbleUntilSorted(bubblePass(xs), n - 1)
    
    bubbleUntilSorted(l, l.length)
  }
}
