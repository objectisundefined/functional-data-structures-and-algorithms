package fpinscala.algorithms.sorting

object MergeSort {
  def merge[T](l1: List[T], l2: List[T])(implicit ev: T => Ordered[T]): List[T] = (l1, l2) match {
    case (_, Nil) => l1
    case (Nil, _) => l2
    case (x :: xs, y :: ys) =>
      if (x <= y) x :: merge(xs, l2) else y :: merge(l1, ys)
  }

  /*
  def mergeSort[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = l match {
    case Nil => Nil
    case _ :: Nil => l
    case _ =>
      val i = (l.length + 1) / 2
      val (f, r) = l.splitAt(i)

      merge(mergeSort(f), mergeSort(r))
  }
  */

  def split[T](l : List[T])(implicit ev: T => Ordered[T]): (List[T], List[T]) = l match {
    case Nil => (Nil, Nil)
    case _ :: Nil => (l, Nil)
    case x :: a :: xs =>
      val (f, r) = split(xs)

      (x :: f, a :: r)
  }

  def mergeSort[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = l match {
    case Nil => Nil
    case _ :: Nil => l
    case _ =>
      val (f, r) = split(l)

      merge(mergeSort(f), mergeSort(r))
  }

}
