package fpinscala.datastructures.graphs

import org.scalatest.{ WordSpec, Matchers }

class GraphSpec extends WordSpec with Matchers {
  import fpinscala.datastructures.graphs.Graph._

  val graph = List(
    ("m", "n"), ("m", "o"), ("m", "p"),
    ("o", "r"),
    ("n", "q"),
    ("p", "q"),
    ("q", "r"), ("q", "s")
  )

  "succSet" should {
    "works" in {
      succSet("m", graph) shouldEqual List("n", "o", "p")
    }
  }

  "depthFirst" should {
    "works" in {
      depthFirst("m", graph) shouldEqual List("m", "n", "q", "r", "s", "o", "p")
    }
  }

  "topsort" should {
    "works" in {
      topsort(graph) shouldEqual List("m", "n", "o", "p", "q", "r", "s")
    }
  }

  val grwork = List(
    ("getup","shower"),
    ("shower", "breakfast"),

    ("breakfast","dress"),
    ("dress","office"),
    ("office", "dinner"),

    ("breakfast","leisurely_lunch"),
    ("leisurely_lunch", "movie"),
    ("movie", "breakfast")
  )

  "topsortWithCycle" should {
    "throw Exception on cycle graph" in  {
      intercept[java.lang.RuntimeException] {
        topsortWithCycle(grwork)
      }
    }
  }

  "topsortPrintCycle" should {
    "works" in {
      topsortPrintCycle(grwork) shouldEqual (
        List("getup", "shower", "breakfast", "dress", "office", "dinner", "leisurely_lunch", "movie"),
        List("breakfast")
      )
    }
  }

  "traverseGraph" should {
    "works" in {
      traverseGraph(grwork) shouldEqual List("getup", "shower", "breakfast", "dress", "office", "dinner", "leisurely_lunch", "movie")
    }
  }

  "cycled" should {
    "works" in {
      cycled(graph) shouldEqual false
      cycled(grwork) shouldEqual true
    }
  }

  "foldl" should {
    "works" in {
      foldl(grwork)(List[String]())((acc, x) => acc ++ List(x)) shouldEqual
      List("getup", "shower", "breakfast", "dress", "office", "dinner", "leisurely_lunch", "movie")
    }
  }

}
