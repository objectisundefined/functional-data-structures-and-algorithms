package fpinscala.algorithms.sorting

object BubbleSort {
  def getLargest[T](l: List[T])(implicit ev: T => Ordered[T]): (T, List[T]) = l match {
    case Nil => (null.asInstanceOf[T], Nil)
    case x :: Nil => (x, Nil)
    case x :: xs =>
      val (a, r) = getLargest(xs)
      if (x >= a) (x, a :: r)
      else (a, x :: r)
  }

  def bubbleSort[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = l match {
    case Nil => Nil
    case _ =>
      val (a, r) = getLargest(l)
      bubbleSort(r) ::: List(a)
  }

}
