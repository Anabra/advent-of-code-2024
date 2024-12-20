package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.Dijkstra
import adventofcode2024.common.graphs.Dijkstra.{Move, SingleRouteResult}

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object Day18 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  def readInput(path: String): Vector[Coords] = {
    val bufferedSource = io.Source.fromResource(path)
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines.map { line =>
      line.split(",") match {
        case Array(x, y) => Coords(x.toInt, y.toInt)
      }
    }
  }

  case class MemorySection(
    dimX: Int,
    dimY: Int,
    corruptedCoords: Set[Coords],
  )

  def getNeighbours(memorySection: MemorySection, prevMove: Move[Coords]): Set[Move[Coords]] = {
    val directions = Set(
      Coords(0, 1),
      Coords(0, -1),
      Coords(1, 0),
      Coords(-1, 0),
    )
    directions
      .map(prevMove.to + _)
      .filter(_.isWithinBounds(memorySection.dimX, memorySection.dimY))
      .diff(memorySection.corruptedCoords)
      .map(coords => Move(prevMove.to, coords, prevMove.cost + 1))
  }

  def findOptimalRoute(
    dimX: Int,
    dimY: Int,
    start: Coords,
    end: Coords,
    corruptedCoords: Set[Coords],
  ): Option[Vector[Coords]] = {
    val result = Dijkstra.exploreSingleOptimalRoute[MemorySection, Coords](
      MemorySection(dimX, dimY, corruptedCoords),
      Coords(0,0),
      (node: Coords) => node == Coords(dimX - 1, dimY - 1),
      getNeighbours
    )

    result.flatMap(_.traceBackSingleOptimalPath)
  }

  def prettyMemorySection(dimX: Int, dimY: Int, corruptedBlocks: Set[Coords], path: Vector[Coords] = Vector()): String = {
    Coords.rangeExclusive(Coords(0, 0), Coords(dimX, dimY))
      .map { coords =>
        if (corruptedBlocks.contains(coords)) {
          '#'
        } else if (path.contains(coords)) {
          'O'
        } else {
          '.'
        }
      }
      .grouped(dimX)
      .map(_.mkString)
      .mkString("\n")
  }

  def task1(): Int = {
    val fallingByteCoords = readInput("day18.txt").take(1024).toSet
    val (dimX, dimY) = (71, 71)
    val start = Coords(0, 0)
    val end = Coords(dimX - 1, dimY - 1)

    println(prettyMemorySection(dimX, dimY, fallingByteCoords))
    println()

    val Some(optimalRoute) = findOptimalRoute(dimX, dimY, start, end, fallingByteCoords)

    println(prettyMemorySection(dimX, dimY, fallingByteCoords, optimalRoute))
    println()

    optimalRoute.size
  }

  def findFirstBlockingByte(
    dimX: Int,
    dimY: Int,
    start: Coords,
    end: Coords,
    corruptedCoords: Vector[Coords],
  ): Option[Coords] = {
    val (baseBlocks, blocksToBecomeCorrupted) = corruptedCoords.splitAt(1024)
    val Some(baseRoute) = findOptimalRoute(dimX, dimY, start, end, baseBlocks.toSet)

    @tailrec
    def loop(
      curRouteCoords: Set[Coords],
      corruptedBlocks: Set[Coords],
      blocksToBecomeCorrupted: Vector[Coords]
    ): Option[Coords] = {
      blocksToBecomeCorrupted match {
        case Vector() => None
        case Vector(nextBlock, rest*) =>
          if (curRouteCoords.contains(nextBlock)) {
            val newBlocks = corruptedBlocks + nextBlock
            findOptimalRoute(dimX, dimY, start, end, newBlocks) match {
              case None =>
                Some(nextBlock)
              case Some(newRoute) =>
                loop(newRoute.toSet, newBlocks, rest.toVector)
            }
          } else {
            loop(curRouteCoords, corruptedBlocks + nextBlock, rest.toVector)
          }
      }
    }

    loop(baseRoute.toSet, baseBlocks.toSet, blocksToBecomeCorrupted)
  }

  def task2(): Option[Coords] = {
    val fallingByteCoords = readInput("day18.txt")
    val (dimX, dimY) = (71, 71)
    val start = Coords(0, 0)
    val end = Coords(dimX - 1, dimY - 1)

    findFirstBlockingByte(dimX, dimY, start, end, fallingByteCoords)
  }
}
