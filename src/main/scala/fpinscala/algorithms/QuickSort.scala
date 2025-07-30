package fpinscala.algorithms.sorting

object QuickSort {
  // Original partition function - keeping for comparison
  def partition[T](e: T, l: List[T], fp: List[T], sp: List[T])(implicit ev: T => Ordered[T]): (List[T], List[T]) = l match {
    case Nil => (fp, sp)
    case x :: xs => if (x < e) partition(e, xs, x :: fp, sp) else partition(e, xs, fp, x :: sp)
  }

  // More functional partition using built-in List operations
  def partitionFunctional[T](pivot: T, l: List[T])(implicit ev: T => Ordered[T]): (List[T], List[T]) =
    l.partition(_ < pivot)

  def quickSort[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = l match {
    case Nil => Nil
    case _ :: Nil => l
    case x :: xs =>
      val (f, r) = partition(x, xs, List[T](), List[T]())
      quickSort(f) ::: x :: quickSort(r)
  }

  // Cleaner functional implementation using partition and flatMap
  def quickSortFunctional[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = l match {
    case Nil => Nil
    case _ :: Nil => l
    case pivot :: xs =>
      val (smaller, larger) = partitionFunctional(pivot, xs)
      quickSortFunctional(smaller) ::: pivot :: quickSortFunctional(larger)
  }

  // Even more functional using filter (commented original approach)
  def quickSortFilter[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = l match {
    case Nil => Nil
    case _ :: Nil => l
    case x :: xs =>
      quickSortFilter(xs filter(_ < x)) ::: x :: quickSortFilter(xs filter(_ >= x))
  }

  // Randomized quicksort for better average performance
  def quickSortRandomized[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = {
    def randomPivot[A](xs: List[A]): (A, List[A]) = {
      val randomIndex = scala.util.Random.nextInt(xs.length)
      val pivot = xs(randomIndex)
      val rest = xs.zipWithIndex.filter(_._2 != randomIndex).map(_._1)
      (pivot, rest)
    }
    
    l match {
      case Nil => Nil
      case _ :: Nil => l
      case _ =>
        val (pivot, rest) = randomPivot(l)
        val (smaller, larger) = partitionFunctional(pivot, rest)
        quickSortRandomized(smaller) ::: pivot :: quickSortRandomized(larger)
    }
  }
}
