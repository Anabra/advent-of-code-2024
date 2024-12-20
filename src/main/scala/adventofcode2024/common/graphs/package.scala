package adventofcode2024.common

import scala.annotation.tailrec

package object graphs {

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
