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
          todo.enqueueAll(getNeighbours(curPos, heightmap) -- visited)
          loop(reachablePeaks, visited + curPos)
        }
      }
    }

    loop(Set.empty, Set.empty)
  }

  def task1(): Int = {
    val heightmap = readInput()
    val trailheads = findTrailheads(heightmap)

    trailheads.flatMap(pos => calcTrailScore(heightmap, pos)).size
  }

  def task2(): Int = {
    val heightmap = readInput()
    42
  }
}
