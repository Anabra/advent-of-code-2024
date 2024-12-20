package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.Dijkstra
import adventofcode2024.common.graphs.Dijkstra.Move

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object Day19 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  type Remainder = String
  type DesignPart = String

  case class Input(
    designParts: Vector[DesignPart],
    fullDesigns: Vector[String],
  )

  def readInput(path: String): Input = {
    val bufferedSource = io.Source.fromResource(path)
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val designParts = lines.head.split(", ").toVector
    val fullDesigns = lines.drop(2)

    Input(designParts, fullDesigns)
  }

  def calcNextMoves(designParts: Vector[String], prevMove: Move[Remainder]): Set[Move[Remainder]] = {
    val remainder = prevMove.to
    val matchingParts = designParts.filter(part => remainder.startsWith(part))
    val res = matchingParts.map { part =>
      Move(
        from = prevMove.to,
        to = remainder.drop(part.length),
        cost = 0,
      )
    }.toSet
    res
  }

  def task1(): Int = {
    val input = readInput("day19.txt")

    println(input.designParts)
    println(input.fullDesigns)

    val solutions = input.fullDesigns.flatMap { fullDesign =>
      Dijkstra.exploreSingleOptimalRoute[Vector[DesignPart], Remainder](
        graph = input.designParts,
        start = fullDesign,
        isEnd = (rem: Remainder) => rem.isEmpty,
        nextMoves = calcNextMoves
      )
    }

    solutions.size
  }

  def tracebackSolution(remainders: Vector[Remainder]): Vector[DesignPart] = {
    remainders.zip(remainders.drop(1)).map { case (prev, next) =>
      prev.dropRight(next.size)
    }
  }

  def task2(): Long = {
    val input = readInput("day19.txt")

    val solutions = input.fullDesigns.map { fullDesign =>
      val solution = Dijkstra.exploreAllOptimalRoutes[Vector[DesignPart], Remainder](
        graph = input.designParts,
        start = fullDesign,
        isEnd = (rem: Remainder) => rem.isEmpty,
        nextMoves = calcNextMoves
      )

      fullDesign -> solution
    }

    lazy val solutionsRoutesComputed = solutions.map { case (des, solOpt) =>
      des -> solOpt.toVector.flatMap(s => s.routeIterator.map(tracebackSolution))
    }

    solutions.map { case (in, solutionOpt) =>
      solutionOpt.fold(0L) { s => s.numRoutes }
    }.sum
  }
}
// solutions(5)._2.get.routes.size
