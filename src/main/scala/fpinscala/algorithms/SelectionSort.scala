package fpinscala.algorithms.sorting

object SelectionSort {

  /*
  def selectionSort[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = l match {
    case Nil => Nil
    case x :: Nil => l
    case x :: xs =>
      val a = xs.min
      if (x <= a) return x :: selectionSort(xs)
      val i = xs.indexOf(a)
      val (f, r) = xs.splitAt(i)
      // `f ::: x :: r.tail` was the same as `f ::: (x :: r.tail)`
      a :: selectionSort(f ::: x :: r.tail)
  }
  */

  def selectionSort[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = l match {
    case Nil => Nil
    case x :: Nil => l
    case _ =>
      val a = l.min

      return (l filter (_ == a)) ::: selectionSort(l filter (_ != a))
  }
}
