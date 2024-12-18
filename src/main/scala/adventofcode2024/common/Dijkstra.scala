package adventofcode2024.common

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


  def findOptimalRoute[Graph, Node](
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
}
