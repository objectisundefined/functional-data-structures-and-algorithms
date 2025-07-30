package fpinscala.algorithms.sorting

object SelectionSort {

  // Find minimum element and remove it from the list
  def extractMin[T](l: List[T])(implicit ev: T => Ordered[T]): Option[(T, List[T])] = l match {
    case Nil => None
    case x :: xs =>
      val minElem = l.min
      val (before, after) = l.span(_ != minElem)
      Some((minElem, before ::: after.tail))
  }

  // Original implementation - functional version without return
  def selectionSort[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = l match {
    case Nil => Nil
    case x :: Nil => l
    case _ =>
      val minElem = l.min
      val remaining = l.filter(_ != minElem)
      val duplicates = l.filter(_ == minElem)
      duplicates ::: selectionSort(remaining)
  }

  // More functional implementation using extractMin
  def selectionSortFunctional[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = 
    extractMin(l) match {
      case None => Nil
      case Some((min, rest)) => min :: selectionSortFunctional(rest)
    }

  // Using fold for a different functional approach
  def selectionSortFold[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = {
    def insertSorted(sorted: List[T], x: T): List[T] = sorted match {
      case Nil => List(x)
      case y :: ys => if (x <= y) x :: sorted else y :: insertSorted(ys, x)
    }
    
    l.foldLeft(List.empty[T])(insertSorted)
  }

  // Selection sort using unfold pattern
  def selectionSortUnfold[T](l: List[T])(implicit ev: T => Ordered[T]): List[T] = {
    def unfold[A, B](seed: A)(f: A => Option[(B, A)]): List[B] = 
      f(seed) match {
        case None => Nil
        case Some((elem, newSeed)) => elem :: unfold(newSeed)(f)
      }
    
    unfold(l)(extractMin(_))
  }
}
