package fpinscala.datastructures.graphs

object Graph extends App {
  def succSet(a: String, g: List[(String, String)]): List[String] = g match {
    case Nil => Nil
    case x :: xs if (a == x._1) => x._2 :: succSet(a, xs)
    case _ :: xs => succSet(a, xs)
  }

  def depthFirst(initial: String, g: List[(String, String)]): List[String] = {
    def rec(nodes: List[String], visited: List[String]): List[String] = nodes match {
      case Nil => visited
      case x :: xs if visited.contains(x) => rec(xs, visited)
      case x :: xs => rec(succSet(x, g) ++ xs, x :: visited)
    }

    rec(List(initial), List()).reverse
  }

  def depthFirst1(initial: String, g: List[(String, String)]): List[String] = ???

  def topsort(g: List[(String, String)]) = ???

  def topsortWithCycle(g: List[(String, String)]) = ???

  type VC = (List[String], List[String])

  def addToVisited(x: String, v: VC) = ???

  def topsortPrintCycle(g: List[(String, String)]) = ???

  val graph = List(("m", "n"), ("m", "o"), ("m", "p"),
    ("n", "q"), ("o", "r"), ("p", "q"),
    ("q", "r"), ("q", "s"))

  println(graph, succSet("m", graph))

  val nodes = depthFirst("m", graph)
  println(nodes)
  // List(m, n, q, r, s, o, p)

  val grwork = List(("getup","shower"),
    ("shower", "breakfast"),
    ("breakfast","dress"),
    ("dress","office"),
    ("office", "dinner"),

    ("breakfast","leisurely_lunch"),
    ("leisurely_lunch", "movie"),
    ("movie", "dinner"))

  // val topsorted = topsortPrintCycle(("dinner", "movie") :: grwork)
  // println(topsorted)

}
