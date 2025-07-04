package fpinscala.algorithms.sorting

object InsertionSort {
  def insertionSort[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = {
    /*
    def rec[T](r: List[T], l: List[T])(implicit ev: T => Ordered[T]): List[T] = l match {
      case Nil => r
      case x :: xs =>
        // use <= to make it stable
        rec((r filter (_ <= x)) ::: x :: (r filter (_ = x)), xs)
    }

    rec(Nil, l)
    */

    def insert[T](l: List[T], e: T)(implicit ev: T => Ordered[T]): List[T] = l match {
      case Nil => List(e)
      // use <= to make it stable
      case x :: xs => if (x <= e) x :: insert(xs, e) else e :: l
    }

    l.foldLeft[List[T]](List[T]())(insert)
  }

}
