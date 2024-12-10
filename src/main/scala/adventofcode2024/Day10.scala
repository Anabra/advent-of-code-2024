package adventofcode2024

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object Day10 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  def readInput(): Vector[Vector[Int]] = {
    val bufferedSource = io.Source.fromResource("day10.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines.map(_.toVector.map(_.asDigit))
  }

  def findTrailheads(heightmap: Vector[Vector[Int]]): Vector[(Int, Int)] =
    heightmap.zipWithIndex.flatMap { case (row, rowIx) =>
      row.zipWithIndex
        .filter { case (height, _) => height == 0 }
        .map { case (_, colIx) => (rowIx, colIx)}
    }

  def getNeighbours(pos: (Int, Int), heightmap: Vector[Vector[Int]]): Set[(Int, Int)] = {
    val (x, y) = pos
    val maxX = heightmap.length
    val maxY = heightmap.head.length
    val curHeight = heightmap(x)(y)

    Set(
      (x - 1, y),
      (x + 1, y),
      (x, y - 1),
      (x, y + 1),
    ).filter { case (nx, ny) => nx >= 0 && nx < maxX && ny >= 0 && ny < maxY }
      .collect { case (nx, ny) if heightmap(nx)(ny) == curHeight + 1 => (nx, ny)}
  }

  def calcTrailScore(heightmap: Vector[Vector[Int]], startingPos: (Int, Int)): Set[(Int, Int)] = {
    val todo = mutable.Queue(startingPos)

    @tailrec
    def loop(reachablePeaks: Set[(Int, Int)], visited: Set[(Int, Int)]): Set[(Int, Int)] = {
      if (todo.isEmpty) {
        reachablePeaks
      } else {
        val curPos@(curX, curY) = todo.dequeue()
        if (visited.contains(curPos)) {
          loop(reachablePeaks, visited)
        } else if (heightmap(curX)(curY) == 9) {
          loop(reachablePeaks + curPos, visited + curPos)
        } else {
          // NOTE: due to the nature of the graph, lower points will always be visited first
          // so we wouldn't even need to check against the visited set here
          todo.enqueueAll(getNeighbours(curPos, heightmap) -- visited)
          loop(reachablePeaks, visited + curPos)
        }
      }
    }

    loop(Set.empty, Set.empty)
  }

  // 566
  def task1(): Int = {
    val heightmap = readInput()
    val trailheads = findTrailheads(heightmap)

    trailheads.flatMap(pos => calcTrailScore(heightmap, pos)).size
  }

  def calcTrailRating(heightmap: Vector[Vector[Int]], startingPos: (Int, Int)): Int = {
    val nonExistentPos = (-1, -1)
    val todo = mutable.Queue(nonExistentPos -> startingPos)
    val numTrailsLeadingToPos = mutable.Map[(Int, Int), Int]()

    while (todo.nonEmpty) {
      val (prevPos, curPos@(curX, curY)) = todo.dequeue()
      val numTrailsLeadingToPrev = numTrailsLeadingToPos.getOrElse(prevPos, 1)
      val numTrailsLeadingToCur = numTrailsLeadingToPos.getOrElse(curPos, 0)

      val alreadyVisitedCurPos = numTrailsLeadingToCur > 0

      // NOTE: due to the nature of the graph, lower points will always be visited first
      // thus there's no need to retraverse the graph
      if (alreadyVisitedCurPos) {
        numTrailsLeadingToPos(curPos) = numTrailsLeadingToCur + numTrailsLeadingToPrev
      } else {
        numTrailsLeadingToPos(curPos) = numTrailsLeadingToPrev
        val neighbours = getNeighbours(curPos, heightmap)
        todo.enqueueAll(neighbours.map(curPos -> _))
      }
    }

    numTrailsLeadingToPos.collect { case ((x,y), numTrails) if heightmap(x)(y) == 9  =>
      numTrails
    }.sum
  }

  def task2(): Int = {
    val heightmap = readInput()
    val trailheads = findTrailheads(heightmap)

    trailheads.map(pos => calcTrailRating(heightmap, pos)).sum
  }
}
