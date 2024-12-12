package adventofcode2024

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object Day12 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  type Garden = Vector[Vector[Char]]
  type Position = (Int, Int)

  case class Exploration(
    flowerType: Char,
    internalPoints: Set[Position],
    externalNeighbours: Set[(Position, Position)],
  ) {
    def externalNeighboursWithinBounds(garden: Garden): Set[Position] =
      externalNeighbours.map(_._2).filter { case (x, y) =>
        x >= 0 && x < garden.length && y >= 0 && y < garden.head.length
      }
  }

  def readInput(): Garden = {
    val bufferedSource = io.Source.fromResource("day12.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines.map(_.toVector)
  }

  def exploreNeighbours(garden: Garden, pos: (Int, Int)): (Set[Position], Set[(Position, Position)]) = {
    val (x, y) = pos
    val flower = garden(x)(y)
    val neighbours = Set(
      (x - 1, y),
      (x + 1, y),
      (x, y - 1),
      (x, y + 1),
    )

    val internalNeighbours = neighbours.filter { case (x, y) =>
      x >= 0 && x < garden.length && y >= 0 && y < garden.head.length && garden(x)(y) == flower
    }
    val externalNeighbours = (neighbours -- internalNeighbours).map((pos, _))

    (internalNeighbours, externalNeighbours)
  }

  def exploreRegion(garden: Garden, pos: Position): Exploration = {
    val todo = mutable.Queue(pos)
    val flower = garden(pos._1)(pos._2)

    def loop(
      visitedInternalPoints: Set[Position],
      seenExternalNeighbours: Set[(Position, Position)])
    : Exploration = {
      if (todo.isEmpty) {
        Exploration(flower, visitedInternalPoints, seenExternalNeighbours)
      } else {
        val currentPos = todo.dequeue()
        if (visitedInternalPoints.contains(currentPos)) {
          loop(visitedInternalPoints, seenExternalNeighbours)
        } else {
          val (internalNeighbours, externalNeighbours) = exploreNeighbours(garden, currentPos)

          todo.enqueueAll(internalNeighbours -- visitedInternalPoints)
          loop(visitedInternalPoints + currentPos, seenExternalNeighbours ++ externalNeighbours)
        }
      }
    }

    loop(Set(), Set())
  }

  def exploreAllRegions(garden: Garden): Vector[Exploration] = {
    val positionsToExplore = mutable.Queue((0, 0))

    def loop(
      explorationsSoFar: Vector[Exploration],
      visitedPositions: Set[Position],
    ): Vector[Exploration] = {
      if (positionsToExplore.isEmpty) {
        explorationsSoFar
      } else {
        val currentPos = positionsToExplore.dequeue()
        if (visitedPositions.contains(currentPos)) {
          loop(explorationsSoFar, visitedPositions)
        } else {
          val exploration = exploreRegion(garden, currentPos)
          val externalNeighboursToExplore = exploration.externalNeighboursWithinBounds(garden) -- visitedPositions
          positionsToExplore.enqueueAll(externalNeighboursToExplore)
          loop(explorationsSoFar :+ exploration, visitedPositions ++ exploration.internalPoints)
        }
      }
    }

    loop(Vector(), Set())
  }

  def calcGardenPrice(exploration: Exploration): Int = {
    val perimeter = exploration.externalNeighbours.size
    val area = exploration.internalPoints.size
    perimeter * area
  }

  def task1(): Int = {
    val garden = readInput()
    val explorations = exploreAllRegions(garden)
    explorations.map(calcGardenPrice).sum
  }

  // selects the right coordinate
  def projectIntoDirection(pos: Position, dir: (Int, Int)): Int = {
    val (x, y) = pos
    val (dx, dy) = dir
    x * dx.abs + y * dy.abs
  }

  def calcNumContinuousSections(sortedNums: Vector[Int]): Int = {
    val numJumps = sortedNums.zip(sortedNums.drop(1))
      .map { case (a, b) => b - a }
      .count(diff => diff > 1)

    numJumps + 1
  }

  def calcPerimeterInDirection(exploration: Exploration, dir: (Int, Int)): Int = {
    val externalNeighboursInGivenDirection = exploration.externalNeighbours
      .collect { case ((fromX, fromY), external@(toX, toY))
        if dir == (toX - fromX, toY - fromY) => external
      }

    val groupedNeighbours = externalNeighboursInGivenDirection
      .toVector
      .sortBy(projectIntoDirection(_, dir.swap))
      .groupBy(pos => projectIntoDirection(pos, dir))

    groupedNeighbours
      .view
      .mapValues { positions =>
        val projectedPositions = positions.map(pos => projectIntoDirection(pos, dir.swap))
        calcNumContinuousSections(projectedPositions)
      }
      .values
      .sum
  }

  def calcGardenPriceWithDiscount(exploration: Exploration): Int = {
    val directions = Vector(
      (1,  0),
      (-1, 0),
      (0,  1),
      (0, -1),
    )
    val perimeter = directions.map(calcPerimeterInDirection(exploration, _)).sum
    val area = exploration.internalPoints.size

    perimeter * area
  }

  def task2(): Int = {
    val garden = readInput()
    val explorations = exploreAllRegions(garden)
    explorations.map(calcGardenPriceWithDiscount).sum
  }
}
