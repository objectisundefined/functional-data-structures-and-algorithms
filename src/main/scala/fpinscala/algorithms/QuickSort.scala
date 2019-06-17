package fpinscala.algorithms.sorting

object QuickSort {
  def partition[T <% Ordered[T]](e: T, l: List[T], fp: List[T], sp: List[T]): (List[T], List[T]) = l match {
    case Nil => (fp, sp)
    case x :: xs => if (x < e) partition(e, xs, x :: fp, sp) else partition(e, xs, fp, x :: sp)
  }

  def quickSort[T <% Ordered[T]](l: List[T]): List[T] = l match {
    case Nil => Nil
    case _ :: Nil => l
    case x :: xs =>
      val (f, r) = partition(x, xs, List[T](), List[T]())

      quickSort(f) ::: x :: quickSort(r)
  }

  /*
  def quickSort[T <% Ordered[T]](l: List[T]): List[T] = l match {
    case Nil => Nil
    case _ :: Nil => l
    case x :: xs =>

      quickSort(xs filter(_ < x)) ::: x :: quickSort(xs filter(_ >= x))
  }
  */

}
