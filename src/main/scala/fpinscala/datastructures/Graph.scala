package fpinscala.datastructures.graphs

object Graph {
  def succSet(a: String, g: List[(String, String)]): List[String] = g match {
    case Nil => Nil
    case x :: xs if (a == x._1) => x._2 :: succSet(a, xs)
    case _ :: xs => succSet(a, xs)
  }

  /**
  def depthFirst(initial: String, g: List[(String, String)]): List[String] = {
    def depthf(nodes: List[String], visited: List[String]): List[String] = nodes match {
      case Nil => visited
      case x :: xs if visited.contains(x) => depthf(xs, visited)
      case x :: xs => depthf(succSet(x, g) ++ xs, x :: visited)
    }

    depthf(List(initial), List()).reverse
  }
  */

  /**
  def depthFirst(initial: String, g: List[(String, String)]): List[String] = {
    def depthf(nodes: List[String], visited: List[String]): List[String] = nodes match {
      case Nil => visited
      case x :: xs => depthf(xs,
        if (visited.contains(x)) visited
        else depthf(succSet(x, g), x :: visited)
      )
    }

    depthf(List(initial), List()).reverse
  }
  */
  def depthFirst(initial: String, g: List[(String, String)]): List[String] = {
    def fn(node: String, visited: List[String]): List[String] = {
      if (visited.contains(node)) visited
      else succSet(node, g).foldLeft(visited ++ List(node))((acc, x) => fn(x, acc))
    }

    fn(initial, List[String]())
  }

  /**
  def topsort(g: List[(String, String)]) = {
    def sort(nodes: List[String], visited: List[String]): List[String] = nodes match {
      case Nil => visited
      case x :: xs => sort(xs,
        if (visited.contains(x)) visited
        else sort(succSet(x, g), x :: visited)
      )
    }

    sort(List(initial), List())
  }
  */
  def topsort(g: List[(String, String)]) = {
    def sort(nodes: List[String], visited: List[String]): List[String] = nodes match {
      case Nil => visited
      case x :: xs => sort(xs,
        if (visited.contains(x)) visited
        else x :: sort(succSet(x, g).reverse, visited))
    }

    val (start, _) = g.unzip
    val result = sort(start, List())
    result
  }

  def topsortWithCycle(g: List[(String, String)]) = {
    def sort(nodes: List[String], path: List[String], visited: List[String]): List[String] = nodes match {
      case Nil => visited
      case x :: xs if path.contains(x) =>
        sys.error("cycle detected")
      case x :: xs => sort(xs, path,
        if (visited.contains(x)) visited
        else x :: sort(succSet(x, g).reverse, x :: path, visited))
    }

    val (start, _) = g.unzip
    val result = sort(start, List(), List())
    result
  }

  type VC = (List[String], List[String])

  def addToVisited(x: String, v: VC) = (x :: v._1, v._2)

  def topsortPrintCycle(g: List[(String, String)]) = {
    def sort(nodes: List[String], path: List[String], visited: VC): VC = nodes match {
      case Nil => visited
      case x :: xs =>
        val (v, c) = visited
        sort(xs, path,
          if (path.contains(x)) (v, x::c)
          else if (v.contains(x)) visited
          else addToVisited(x, sort(succSet(x, g).reverse, x :: path, visited))
        )
    }

    val (start, _) = g.unzip
    val result = sort(start, List(), (List(), List()))
    result
  }

  def cycled(g: List[(String, String)]): Boolean = {
    def fn(node: String, visited: List[String]): Boolean = {
      if (visited.contains(node)) true
      else succSet(node, g).exists(fn(_, node :: visited))
    }

    val (start, _) = g.unzip
    val result = start.exists(fn(_, List[String]()))
    result
  }

  def traverseGraph(g: List[(String, String)]) = {
    def fn(node: String, visited: List[String]): List[String] = {
      if (visited.contains(node)) visited
      else succSet(node, g).foldLeft(visited ++ List(node))((acc, x) => fn(x, acc))
    }

    val (start, _) = g.unzip
    val result = start.foldLeft(List[String]())((acc, x) => fn(x, acc))
    result
  }

  def foldl[A](g: List[(String, String)])(z: A)(f: (A, String) => A) = {
    def fn(node: String, visited: (List[String], A)): (List[String], A) = {
      if (visited._1.contains(node)) visited
      else succSet(node, g).foldLeft(
        (visited._1 ++ List(node), f(visited._2, node)))
        { (acc: (List[String], A), x: String) => fn(x, acc) }
    }

    val (start, _) = g.unzip
    val result = start.foldLeft((List[String](), z))((acc, x) => fn(x, acc))
    result._2
  }

}
