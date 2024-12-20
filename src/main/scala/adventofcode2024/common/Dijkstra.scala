package adventofcode2024.common

import adventofcode2024.common.Dijkstra.Move

import scala.annotation.tailrec
import scala.collection.mutable

// TODO: geeralize the solution for finding all paths
object Dijkstra {
  type Cost = Long

  case class Move[Node](
    from: Node,
    to: Node,
    cost: Cost,
  )

  case class SingleRouteResult[Node](
    end: Node,
    visits: Map[Node, Move[Node]]
  ) {
    def cost: Cost = visits(end).cost

    def traceBackSingleOptimalPath: Option[Vector[Node]] = {
      @tailrec
      def loop(curNode: Node, path: Vector[Node]): Vector[Node] = {
        if (!visits.contains(curNode)) {
          path
        } else {
          val curMove = visits(curNode)
          loop(curMove.from, curNode +: path)
        }
      }

      if (visits.contains(end)) {
        Some(loop(end, Vector.empty))
      } else {
        None
      }
    }
  }

  case class MultiRouteResult[Node](
    ends: Set[Node], // non-empy
    visits: Map[Node, Set[Move[Node]]],
  ) {
    def cost: Cost = visits(ends.head).head.cost

    def routeIterator: RouteTracerIterator[Node] = ends
      .map(end => RouteTracerIterator(end, visits))
      .reduce((lhs, rhs) => (lhs ++ rhs).asInstanceOf[RouteTracerIterator[Node]])

    /**
     * Same as `routeIterator.size`, but much more efficient. It doesn't materialize routes.
     */
    def numRoutes: Long = ends
      .map(end => RouteTracerIterator(end, visits).numRoutes)
      .sum
  }

  def exploreSingleOptimalRoute[Graph, Node](
    graph: Graph,
    start: Node,
    isEnd: Node => Boolean,
    nextMoves: (Graph, Move[Node]) => Set[Move[Node]],
  ): Option[SingleRouteResult[Node]] = {
    val fictionalStartMove = Move(start, start, 0L)
    val todo = mutable.PriorityQueue[Move[Node]](fictionalStartMove)(Ordering.by[Move[Node], Cost](_.cost).reverse)

    @tailrec
    def loop(visited: Map[Node, Move[Node]], endOpt: Option[Node]): Option[SingleRouteResult[Node]] = {
      if (todo.isEmpty) {
        endOpt.map(end => SingleRouteResult(end, visited))
      } else {
        val curMove = todo.dequeue()
        visited.get(curMove.to) match {
          case Some(_) =>
            loop(visited, endOpt)
          case None =>
            if (isEnd(curMove.to)) {
              Some(SingleRouteResult(curMove.to, visited.updated(curMove.to, curMove)))
            } else {
              val neighbours = nextMoves(graph, curMove)
              todo.enqueue(neighbours.toSeq *)
              loop(visited.updated(curMove.to, curMove), endOpt)
            }
        }
      }
    }

    val resultOpt = loop(Map.empty, None)
    resultOpt.map { result =>
      result.copy(visits = result.visits - start) // to account for the fictional init move
    }
  }

  // TODO: test this with Day16
  def exploreAllOptimalRoutes[Graph, Node](
    graph: Graph,
    start: Node,
    isEnd: Node => Boolean,
    nextMoves: (Graph, Move[Node]) => Set[Move[Node]],
  ): Option[MultiRouteResult[Node]] = {
    val fictionalStartMove = Move(start, start, 0L)
    val todo = mutable.PriorityQueue[Move[Node]](fictionalStartMove)(Ordering.by[Move[Node], Cost](_.cost).reverse)

    // TODO: multiple ends?
    @tailrec
    def loop(visits: Map[Node, Set[Move[Node]]], ends: Set[Node]): Option[MultiRouteResult[Node]] = {
      if (todo.isEmpty) {
        if (ends.nonEmpty) {
          Some(MultiRouteResult(ends, visits))
        } else {
          None
        }
      } else {
        val curMove = todo.dequeue()
        visits.get(curMove.to) match {
          case Some(prevMoves) =>
            if (prevMoves.forall(_.cost == curMove.cost)) {
              val newVisits = visits.updated(curMove.to, prevMoves + curMove)
              loop(newVisits, ends)
            } else if (prevMoves.exists(_.cost > curMove.cost)) {
              throw new IllegalStateException("When we visit a node for the first time, it should be on an optimal path. However, we found a new 'more optimal' path to an already visited node.")
            } else {
              loop(visits, ends)
            }
          case None =>
            val newVisits = visits.updated(curMove.to, Set(curMove)) // safe, wasnt in it previously
            if (isEnd(curMove.to)) {
              loop(newVisits, ends + curMove.to)
            } else {
              val neighbours = nextMoves(graph, curMove)
              todo.enqueue(neighbours.toSeq *)
              loop(newVisits, ends)
            }
        }
      }
    }

    val resultOpt = loop(Map.empty, Set.empty)
    resultOpt.map { result =>
      result.copy(visits = result.visits - start) // to account for the fictional init move
    }
  }
}

class RouteTracerIterator[Node](
  end: Node,
  visits: Map[Node, Set[Move[Node]]],
) extends Iterator[Vector[Node]] {
  private val todo = mutable.Stack[Vector[Node]](Vector(end))

  // TODO: is this correct?
  override def hasNext: Boolean = todo.nonEmpty

  // will throw if called without a `hasNext` guard
  override def next(): Vector[Node] = {
    val curRoute@Vector(curNode, rest*) = todo.pop()
    visits.get(curNode) match {
      case None =>
        curRoute
      case Some(movesToProcess) =>
        val newNodes = movesToProcess.map(_.from)
        val newRoutes = newNodes.map(_ +: curRoute)
        todo.pushAll(newRoutes)
        next()
    }
  }

  /**
   * Much more efficient version of `size`. It doesn't materialize all the routes.
   * It doesn't consume the iterator.
   */
  lazy val numRoutes: Long = {
    // We need to reverse the graph so that we can index into it with `end`
    // `visits` is basically the "reverse graph" of the original graph, so we are restoring the original graph here.
    val revGraph = visits.view.mapValues(_.map(_.from)).toMap
    val originalGraph = Utils.reverseGraph(revGraph)
    val toposortedNodes = Utils.toposort(originalGraph)

    val numRoutesTo = toposortedNodes.foldLeft(Map.empty[Node, Long]) { case (numRoutesFromStartTo, curNode) =>
      val prevNodes = revGraph.getOrElse(curNode, Set.empty)
      val numRoutesToPrevs = prevNodes
        .toVector
        .map(prev => numRoutesFromStartTo.getOrElse(prev, 0L))
        .reduceOption(_ + _)
        .getOrElse(1L) // if it had no parents, it must be a root
      numRoutesFromStartTo.updated(curNode, numRoutesToPrevs)
    }

    numRoutesTo(end)
  }

  override def size: Int = {
    if (numRoutes > Int.MaxValue) {
      throw new Exception("Number of routes can't fit into `Int`. Use `numRoutes` instead")
    } else {
      numRoutes.toInt
    }
  }
}

object Utils {
  def reverseGraph[Node](graph: Map[Node, Set[Node]]): Map[Node, Set[Node]] = {
    val edges = graph.toSet.flatMap { case (parent, children) =>
      children.map(child => child -> parent)
    }

    edges
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .toMap
  }

  // this is a custom BFS-based, tailrec version
  // TODO: optimize data structures
  // TODO: maybe we could use Kahn's algorithm
  def toposort[Node](graph: Map[Node, Set[Node]]): Vector[Node] = {
    val toVisit = collection.mutable.Queue.empty[Node]

    val revGraph = reverseGraph(graph)

    val roots: Set[Node] = {
      val allNodes = graph.keySet
      val allChildren = graph.values.flatten.toSet
      allNodes -- allChildren
    }

    toVisit.enqueueAll(roots)

    @tailrec
    def toposortHelper(sorted: Vector[Node]): Vector[Node] =
      if (toVisit.isEmpty) {
        sorted
      } else {
        val next = toVisit.dequeue()
        lazy val parents = revGraph.getOrElse(next, Vector.empty)
        if (sorted.contains(next) || parents.exists(!sorted.contains(_))) {
          toposortHelper(sorted)
        } else {
          toVisit.enqueueAll(graph.getOrElse(next, Vector.empty))
          toposortHelper(sorted :+ next)
        }
      }

    toposortHelper(Vector.empty)
  }

}
