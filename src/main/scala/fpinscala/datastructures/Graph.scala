package fpinscala.datastructures.graphs

object Graph {
  // ADT for graph representation
  type Graph[A] = List[(A, A)]
  type Edge[A] = (A, A)
  
  // Result types for operations that can fail
  sealed trait GraphResult[+A]
  case class Success[A](value: A) extends GraphResult[A]
  case class CycleDetected[A](cycle: A) extends GraphResult[A]
  case object EmptyGraph extends GraphResult[Nothing]

  // Graph operations
  def empty[A]: Graph[A] = Nil
  
  def addEdge[A](from: A, to: A, graph: Graph[A]): Graph[A] = (from, to) :: graph
  
  def vertices[A](graph: Graph[A]): List[A] = {
    val (sources, targets) = graph.unzip
    (sources ::: targets).distinct
  }
  
  def edges[A](graph: Graph[A]): List[Edge[A]] = graph

  // Get successors of a node - more functional implementation
  def successors[A](node: A, graph: Graph[A]): List[A] = 
    graph.collect { case (from, to) if from == node => to }

  // Get predecessors of a node
  def predecessors[A](node: A, graph: Graph[A]): List[A] = 
    graph.collect { case (from, to) if to == node => from }

  // Check if graph has edge
  def hasEdge[A](from: A, to: A, graph: Graph[A]): Boolean = 
    graph.contains((from, to))

  // Functional depth-first search with State monad pattern
  case class DFSState[A](visited: Set[A], path: List[A], cycles: List[List[A]]) {
    def isVisited(node: A): Boolean = visited.contains(node)
    def visit(node: A): DFSState[A] = copy(visited = visited + node, path = node :: path)
    def detectCycle(node: A): DFSState[A] = {
      val cycleNodes = node :: path.takeWhile(_ != node)
      copy(cycles = cycleNodes :: cycles)
    }
  }

  object DFSState {
    def empty[A]: DFSState[A] = DFSState(Set.empty, Nil, Nil)
  }

  // Enhanced depth-first search with proper state management
  def depthFirstSearch[A](start: A, graph: Graph[A]): List[A] = {
    def dfs(node: A, state: DFSState[A]): DFSState[A] = {
      if (state.isVisited(node)) state
      else {
        val newState = state.visit(node)
        successors(node, graph).foldLeft(newState)((acc, successor) => dfs(successor, acc))
      }
    }
    
    dfs(start, DFSState.empty).path.reverse
  }

  // Breadth-first search using functional queue
  def breadthFirstSearch[A](start: A, graph: Graph[A]): List[A] = {
    def bfs(queue: List[A], visited: Set[A], result: List[A]): List[A] = queue match {
      case Nil => result.reverse
      case node :: rest =>
        if (visited.contains(node)) bfs(rest, visited, result)
        else {
          val neighbors = successors(node, graph).filterNot(visited.contains)
          bfs(rest ::: neighbors, visited + node, node :: result)
        }
    }
    
    bfs(List(start), Set.empty, Nil)
  }

  // Topological sort with cycle detection using DFS post-order
  def topologicalSort[A](graph: Graph[A]): GraphResult[List[A]] = {
    case class TopSortState[A](visited: Set[A], visiting: Set[A], result: List[A])
    
    def dfs(node: A, state: TopSortState[A]): Either[List[A], TopSortState[A]] = {
      if (state.visiting.contains(node)) {
        // Found a cycle
        Left(List(node))
      } else if (state.visited.contains(node)) {
        Right(state)
      } else {
        val newState = state.copy(visiting = state.visiting + node)
        // Visit all successors first
        successors(node, graph).foldLeft[Either[List[A], TopSortState[A]]](Right(newState)) {
          case (Left(cycle), _) => Left(cycle)
          case (Right(currentState), successor) => dfs(successor, currentState)
        } match {
          case Left(cycle) => Left(cycle)
          case Right(finalState) => 
            // Add node to result after visiting all successors (post-order)
            Right(finalState.copy(
              visited = finalState.visited + node,
              visiting = finalState.visiting - node,
              result = node :: finalState.result
            ))
        }
      }
    }
    
    val allVertices = vertices(graph)
    if (allVertices.isEmpty) Success(Nil)
    else {
      val initialState = TopSortState[A](Set.empty, Set.empty, Nil)
      allVertices.foldLeft[Either[List[A], TopSortState[A]]](Right(initialState)) {
        case (Left(cycle), _) => Left(cycle)
        case (Right(state), vertex) => dfs(vertex, state)
      } match {
        case Left(cycle) => CycleDetected(cycle)
        case Right(finalState) => Success(finalState.result)
      }
    }
  }

  // Check if graph has cycles
  def hasCycle[A](graph: Graph[A]): Boolean = {
    def dfs(node: A, visited: Set[A], path: Set[A]): Boolean = {
      if (path.contains(node)) true
      else if (visited.contains(node)) false
      else {
        val newPath = path + node
        val newVisited = visited + node
        successors(node, graph).exists(dfs(_, newVisited, newPath))
      }
    }
    
    vertices(graph).exists(dfs(_, Set.empty, Set.empty))
  }

  // Find strongly connected components using Tarjan's algorithm (functional version)
  def stronglyConnectedComponents[A](graph: Graph[A]): List[List[A]] = {
    case class TarjanState[A](
      index: Int,
      stack: List[A],
      indices: Map[A, Int],
      lowLinks: Map[A, Int],
      onStack: Set[A],
      components: List[List[A]]
    )
    
    def tarjan(node: A, state: TarjanState[A]): TarjanState[A] = {
      if (state.indices.contains(node)) state
      else {
        val nodeIndex = state.index
        val newState = state.copy(
          index = state.index + 1,
          stack = node :: state.stack,
          indices = state.indices + (node -> nodeIndex),
          lowLinks = state.lowLinks + (node -> nodeIndex),
          onStack = state.onStack + node
        )
        
        val afterSuccessors = successors(node, graph).foldLeft(newState) { (acc, successor) =>
          if (!acc.indices.contains(successor)) {
            val result = tarjan(successor, acc)
            result.copy(lowLinks = result.lowLinks + (node -> 
              math.min(result.lowLinks(node), result.lowLinks(successor))))
          } else if (acc.onStack.contains(successor)) {
            acc.copy(lowLinks = acc.lowLinks + (node -> 
              math.min(acc.lowLinks(node), acc.indices(successor))))
          } else acc
        }
        
        if (afterSuccessors.lowLinks(node) == afterSuccessors.indices(node)) {
          val (component, remainingStack) = afterSuccessors.stack.span(_ != node)
          val newComponent = node :: component
          afterSuccessors.copy(
            stack = remainingStack.tail,
            onStack = afterSuccessors.onStack -- newComponent,
            components = newComponent :: afterSuccessors.components
          )
        } else afterSuccessors
      }
    }
    
    val initialState = TarjanState[A](0, Nil, Map.empty, Map.empty, Set.empty, Nil)
    val finalState = vertices(graph).foldLeft(initialState)((state, vertex) => tarjan(vertex, state))
    finalState.components.reverse
  }

  // Shortest path using functional approach (simple BFS-based)
  def shortestPath[A](start: A, end: A, graph: Graph[A]): Option[List[A]] = {
    case class PathState[A](queue: List[(A, List[A])], visited: Set[A])
    
    def bfs(state: PathState[A]): Option[List[A]] = state.queue match {
      case Nil => None
      case (node, path) :: rest =>
        if (node == end) Some((node :: path).reverse)
        else if (state.visited.contains(node)) bfs(PathState(rest, state.visited))
        else {
          val neighbors = successors(node, graph)
            .filterNot(state.visited.contains)
            .map(neighbor => (neighbor, node :: path))
          bfs(PathState(rest ::: neighbors, state.visited + node))
        }
    }
    
    bfs(PathState(List((start, Nil)), Set.empty))
  }

  // Functional fold over graph vertices
  def foldVertices[A, B](graph: Graph[A], initial: B)(f: (B, A) => B): B = {
    def fold(node: A, state: (Set[A], B)): (Set[A], B) = {
      val (visited, acc) = state
      if (visited.contains(node)) state
      else {
        val newAcc = f(acc, node)
        val newVisited = visited + node
        successors(node, graph).foldLeft((newVisited, newAcc))((s, successor) => fold(successor, s))
      }
    }
    
    vertices(graph).foldLeft((Set.empty[A], initial))((state, vertex) => fold(vertex, state))._2
  }

  // Map over graph preserving structure
  def mapGraph[A, B](graph: Graph[A])(f: A => B): Graph[B] =
    graph.map { case (from, to) => (f(from), f(to)) }

  // Filter edges based on predicate
  def filterEdges[A](graph: Graph[A])(predicate: Edge[A] => Boolean): Graph[A] =
    graph.filter(predicate)

  // Legacy string-based functions for backward compatibility
  @deprecated("Use generic versions instead", "2.0")
  def succSet(a: String, g: List[(String, String)]): List[String] = successors(a, g)
  
  @deprecated("Use depthFirstSearch instead", "2.0") 
  def depthFirst(initial: String, g: List[(String, String)]): List[String] = 
    depthFirstSearch(initial, g)

  @deprecated("Use topologicalSort instead", "2.0")
  def topsort(g: List[(String, String)]): List[String] = 
    topologicalSort(g) match {
      case Success(result) => result
      case CycleDetected(_) => sys.error("cycle detected")
      case EmptyGraph => Nil
    }

  @deprecated("Use hasCycle instead", "2.0")
  def cycled(g: List[(String, String)]): Boolean = hasCycle(g)
  
  @deprecated("Use topologicalSort with exception handling", "2.0")
  def topsortWithCycle(g: List[(String, String)]): List[String] = 
    topologicalSort(g) match {
      case Success(result) => result
      case CycleDetected(_) => sys.error("cycle detected")
      case EmptyGraph => Nil
    }
  
  @deprecated("Use topologicalSort instead", "2.0")
  def topsortPrintCycle(g: List[(String, String)]): (List[String], List[String]) = 
    topologicalSort(g) match {
      case Success(result) => (result, Nil)
      case CycleDetected(cycle) => (Nil, cycle)
      case EmptyGraph => (Nil, Nil)
    }
  
  @deprecated("Use depthFirstSearch for each vertex", "2.0")
  def traverseGraph(g: List[(String, String)]): List[String] = {
    val allVertices = vertices(g)
    allVertices.foldLeft(List.empty[String])((acc, vertex) => 
      if (acc.contains(vertex)) acc 
      else acc ::: depthFirstSearch(vertex, g))
  }
  
  @deprecated("Use foldVertices instead", "2.0")
  def foldl[A](g: List[(String, String)])(z: A)(f: (A, String) => A): A = 
    foldVertices(g, z)(f)
}
