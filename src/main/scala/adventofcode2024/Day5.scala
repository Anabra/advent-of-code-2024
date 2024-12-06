package adventofcode2024

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object Day5 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  def readInput(): (Vector[(Int, Int)], Vector[Vector[Int]]) = {
    val bufferedSource = io.Source.fromResource("day5.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val emptyLineIx = lines.indexWhere(_.isEmpty) // wil certainly exist
    val (rawRules, _ +: rawPages) = lines.splitAt(emptyLineIx)

    val rulePattern = """(\d+)\|(\d+)""".r
    val parsedRules = rawRules.map {
      case rulePattern(lhs, rhs) => (lhs.toInt, rhs.toInt)
    }

    val parsedPages = rawPages.map { pages =>
      pages.split(",").map(_.toInt).toVector
    }

    (parsedRules, parsedPages)
  }

  def task1(): Int = {
    val (rules, pageLists) = readInput()
    val ruleSet = rules.toSet
    pageLists.filter { pageList =>
      val pagePairs = pageList.zipWithIndex.flatMap { case (page, ix) =>
        val suffix = pageList.drop(ix + 1)
        suffix.map(suffixPage => (page, suffixPage))
      }
      pagePairs.forall(pagePair => !ruleSet.contains(pagePair.swap))
    }.map { pageList =>
      val numPages = pageList.size
      pageList(numPages / 2)
    }.sum
  }

  def reorder(orderedPages: Vector[Int], unorderedPages: Vector[Int]): Vector[Int] = {
    unorderedPages.map(p => p -> orderedPages.indexOf(p)).sortBy(_._2).map(_._1)
  }

  def mkGraph(edges: Vector[(Int, Int)]): Map[Int, Vector[Int]] = {
    edges.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
  }

  def toposort(graph: Map[Int, Vector[Int]]): Vector[Int] = {
    val toVisit = collection.mutable.Queue.empty[Int]

    val reverseGraph = mkGraph(
      graph.toVector.flatMap { case (parent, children) =>
        children.map(child => child -> parent)
      }
    )

    val roots: Set[Int] = {
      val allNodes = graph.keySet
      val allChildren = graph.values.flatten.toSet
      allNodes -- allChildren
    }

    toVisit.enqueueAll(roots)

    @tailrec
    def toposortHelper(sorted: Vector[Int]): Vector[Int] =
      if (toVisit.isEmpty) {
        sorted
      } else {
        val next = toVisit.dequeue()
        lazy val parents = reverseGraph.getOrElse(next, Vector.empty)
        if (sorted.contains(next) || parents.exists(!sorted.contains(_))) {
          toposortHelper(sorted)
        } else {
          toVisit.enqueueAll(graph.getOrElse(next, Vector.empty))
          toposortHelper(sorted :+ next)
        }
      }

    toposortHelper(Vector())
  }

  def toposortTest(): Unit = {
    val g1 = mkGraph(
      Vector(
        1 -> 5,
        2 -> 5,
        5 -> 3,
        5 -> 7,
        5 -> 8,
        3 -> 4,
        7 -> 4,
        4 -> 10,
        8 -> 6,
      )
    )

    val g2 = mkGraph(
      Vector(
        1 -> 5,
        1 -> 7,
        5 -> 4,
        4 -> 3,
        4 -> 2,
        2 -> 6,
        6 -> 8,
        7 -> 6,
      )
    )

    println(toposort(g1))
    println(toposort(g2))
  }

  def task2(): Int = {
    val (rules, pageLists) = readInput()
    val orderedPages = toposort(mkGraph(rules))

    pageLists.map { unorderedPages =>
      val reorderedPages = reorder(orderedPages, unorderedPages)

      // only consider those page lists that were initially unordered
      if (reorderedPages == unorderedPages) {
        0
      } else {
        val numPages = reorderedPages.size
        reorderedPages(numPages / 2)
      }
    }.sum
  }
}

