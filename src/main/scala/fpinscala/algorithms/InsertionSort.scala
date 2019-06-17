package fpinscala.algorithms.sorting

object InsertionSort {
  def insertionSort[T <% Ordered[T]](l: List[T]): List[T] = {
    /*
    def rec[T <% Ordered[T]](r: List[T], l: List[T]): List[T] = l match {
      case Nil => r
      case x :: xs =>
        // use <= to make it stable
        rec((r filter (_ <= x)) ::: x :: (r filter (_ = x)), xs)
    }

    rec(Nil, l)
    */

    def insert[T <% Ordered[T]](l: List[T], e: T): List[T] = l match {
      case Nil => List(e)
      // use <= to make it stable
      case x :: xs => if (x <= e) x :: insert(xs, e) else e :: l
    }

    l.foldLeft[List[T]](List[T]())(insert)
  }

}
