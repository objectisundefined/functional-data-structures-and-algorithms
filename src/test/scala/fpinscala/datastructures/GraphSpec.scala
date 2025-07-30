package fpinscala.datastructures.graphs

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class GraphSpec extends AnyWordSpec with Matchers {
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
      val result = topsort(graph)
      // Verify it's a valid topological ordering by checking dependencies
      result should contain allOf("m", "n", "o", "p", "q", "r", "s")
      
      // Check each dependency constraint
      val mIdx = result.indexOf("m")
      val nIdx = result.indexOf("n") 
      val oIdx = result.indexOf("o")
      val pIdx = result.indexOf("p")
      val qIdx = result.indexOf("q")
      val rIdx = result.indexOf("r")
      val sIdx = result.indexOf("s")
      
      // Verify the essential dependencies are respected
      if (mIdx != -1 && nIdx != -1) mIdx should be < nIdx
      if (mIdx != -1 && oIdx != -1) mIdx should be < oIdx 
      if (mIdx != -1 && pIdx != -1) mIdx should be < pIdx
      if (oIdx != -1 && rIdx != -1) oIdx should be < rIdx
      if (nIdx != -1 && qIdx != -1) nIdx should be < qIdx
      if (pIdx != -1 && qIdx != -1) pIdx should be < qIdx
      if (qIdx != -1 && rIdx != -1) qIdx should be < rIdx
      if (qIdx != -1 && sIdx != -1) qIdx should be < sIdx
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
      val (result, cycle) = topsortPrintCycle(grwork)
      // Should return empty result list when cycle is detected
      result shouldEqual List()
      // Should detect a cycle (the exact cycle detected may vary)
      cycle should not be empty
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
