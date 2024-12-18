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
    end: Node,
    visits: Map[Node, Set[Move[Node]]],
  ) {
    def cost: Cost = visits(end).head.cost

    def routes: RouteTracerIterator[Node] = RouteTracerIterator(end, visits)
  }

  // TODO: test this with Day16
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

  def exploreAllOptimalRoutes[Graph, Node](
    graph: Graph,
    start: Node,
    isEnd: Node => Boolean,
    nextMoves: (Graph, Move[Node]) => Set[Move[Node]],
  ): Option[MultiRouteResult[Node]] = {
    val fictionalStartMove = Move(start, start, 0L)
    val todo = mutable.PriorityQueue[Move[Node]](fictionalStartMove)(Ordering.by[Move[Node], Cost](_.cost).reverse)

    @tailrec
    def loop(visits: Map[Node, Set[Move[Node]]], endOpt: Option[Node]): Option[MultiRouteResult[Node]] = {
      if (todo.isEmpty) {
        endOpt.map(end => MultiRouteResult(end, visits))
      } else {
        val curMove = todo.dequeue()
        visits.get(curMove.to) match {
          case Some(prevMoves) =>
            if (prevMoves.forall(_.cost == curMove.cost)) {
              val newVisits = visits.updated(curMove.to, prevMoves + curMove)
              loop(newVisits, endOpt)
            } else if (prevMoves.exists(_.cost > curMove.cost)) {
              throw new IllegalStateException("When we visit a node for the first time, it should be on an optimal path. However, we found a new 'more optimal' path to an already visited node.")
            } else {
              loop(visits, endOpt)
            }
          case None =>
            val newVisits = visits.updated(curMove.to, Set(curMove)) // safe, wasnt in it previously
            if (isEnd(curMove.to)) {
              loop(newVisits, Some(curMove.to))
            } else {
              val neighbours = nextMoves(graph, curMove)
              todo.enqueue(neighbours.toSeq *)
              loop(newVisits, endOpt)
            }
        }
      }
    }

    val resultOpt = loop(Map.empty, None)
    resultOpt.map { result =>
      result.copy(visits = result.visits - start) // to account for the fictional init move
    }
  }
}

case class RouteTracerIterator[Node](
  end: Node,
  visits: Map[Node, Set[Move[Node]]],
) extends Iterator[Vector[Node]] {
  private val todo = mutable.Stack[Vector[Node]](Vector(end))

  override def hasNext: Boolean = todo.nonEmpty

  override def next(): Vector[Node] = {
    val curRoute@Vector(curNode, rest*) = todo.pop()
    visits.get(curNode) match {
      case None => curRoute
      case Some(movesToProcess) =>
        val newNodes = movesToProcess.map(_.from)
        val newRoutes = newNodes.map(_ +: curRoute)
        todo.pushAll(newRoutes)
        next()
    }
  }
}
