package adventofcode2024

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object Day6 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  def findStartingCoord(grid: Vector[Vector[Char]]): (Int, Int) =
    grid.zipWithIndex.flatMap { case (row, rowIx) =>
      val colIxOpt = Option(row.indexWhere(_ == '^')).filter(_ >= 0)
      colIxOpt.map(colIx => (rowIx, colIx))
    }.head


  def readInput(): ((Int, Int), Vector[Vector[Char]]) = {
    val bufferedSource = io.Source.fromResource("day6_small_2.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val grid = lines.map(_.toVector)
    val startingCoords@(startingX, startingY) = findStartingCoord(grid)
    (startingCoords, grid.updated(startingX, grid(startingX).updated(startingY, '.')))
  }

  val up = (-1, 0)
  val down = (1, 0)
  val left = (0, -1)
  val right = (0, 1)

  def calcRightTurnBasedOnCurrentFacing(curFacing: (Int, Int)): (Int, Int) = curFacing match {
    case `up` => right
    case `right` => down
    case `down` => left
    case `left` => up
  }

  def calcNextPos(curPos: (Int, Int), direction: (Int, Int)): (Int, Int) = {
    val (curX, curY) = curPos
    val (dx, dy) = direction
    (curX + dx, curY + dy)
  }

  def moveOnGrid(curPos: (Int, Int), direction: (Int, Int), grid: Vector[Vector[Char]]): Option[((Int, Int), (Int, Int))] = {
    val (newX, newY) = calcNextPos(curPos, direction)
    if (newX < 0 || newY < 0 || newX >= grid.size || newY >= grid.head.size) {
      None
    } else {
      grid(newX)(newY) match {
        case '.' =>
          Some((newX, newY), direction)
        case '#' =>
          val relativeRightDirection = calcRightTurnBasedOnCurrentFacing(direction)
          moveOnGrid(curPos, relativeRightDirection, grid)
      }
    }
  }

  @tailrec
  def moveTillCan(
    curPos: (Int, Int),
    curDirection: (Int, Int),
    grid: Vector[Vector[Char]],
    visited: Set[(Int, Int)],
  ): Set[(Int, Int)] = {
    moveOnGrid(curPos, curDirection, grid) match {
      case None => visited
      case Some((newPos, newDirection)) => moveTillCan(newPos, newDirection, grid, visited + newPos)
    }
  }

  def task1(): Int = {
    val (startingCoords, grid) = readInput()
    val visited = moveTillCan(startingCoords, up, grid, Set(startingCoords))
    visited.size
  }

  enum PathResult:
    case Exited, Looping

  @tailrec
  def moveTillCanWithLoopingDetection(
    curPos: (Int, Int),
    curDirection: (Int, Int),
    grid: Vector[Vector[Char]],
    visitedTurns: Set[((Int, Int), (Int, Int))],
  ): PathResult= {
    if (visitedTurns.contains(curPos -> curDirection)) {
      PathResult.Looping
    } else {
      moveOnGrid(curPos, curDirection, grid) match {
        case None => PathResult.Exited
        case Some((newPos, newDirection)) if newDirection == curDirection =>
          moveTillCanWithLoopingDetection(newPos, newDirection, grid, visitedTurns)
        case Some((newPos, newDirection)) if newDirection != curDirection =>
          moveTillCanWithLoopingDetection(newPos, newDirection, grid, visitedTurns + (curPos -> curDirection))
      }
    }
  }

  def moveTillCanWithNewObstacleSimulation(
    startPos: (Int, Int),
    grid: Vector[Vector[Char]],
  ): Set[(Int, Int)] = {
    @tailrec
    def loop(
      curPos: (Int, Int),
      curDirection: (Int, Int),
      grid: Vector[Vector[Char]],
      newObstacleCoords: Set[(Int, Int)],
    ): Set[(Int, Int)] = {
      moveOnGrid(curPos, curDirection, grid) match {
        case None => newObstacleCoords
        case Some((newPos@(newX, newY), newDirection)) if newDirection == curDirection =>
          // Simulate run with new obstacle added from beginning
          val gridWithNewObstacle = grid.updated(newX, grid(newX).updated(newY, '#'))
          val simulationResult = moveTillCanWithLoopingDetection(startPos, up, gridWithNewObstacle, Set.empty)
          val newNewObstacles = if (simulationResult == PathResult.Looping) newObstacleCoords + newPos else newObstacleCoords

          loop(
            newPos,
            newDirection,
            grid,
            newNewObstacles
          )
        case Some((newPos, newDirection)) if newDirection != curDirection => loop(
          newPos,
          newDirection,
          grid,
          newObstacleCoords,
        )
      }
    }

    loop(startPos, up, grid, Set.empty) - startPos
  }

  def calcLeftTurnBasedOnCurrentFacing(curFacing: (Int, Int)): (Int, Int) = curFacing match {
    case `up` => left
    case `left` => down
    case `down` => right
    case `right` => up
  }

  def reverseDirection(dir: (Int, Int)): (Int, Int) = dir match {
    case `up` => down
    case `right` => left
    case `down` => up
    case `left` => right
  }

  def printStats(grid: Vector[Vector[Char]]): Unit = {
    val numObstaclesPerRow = grid.map(_.count(_ == '#'))
    val numRows = grid.size

    println(s"total: ${numObstaclesPerRow.sum}")
    println(s"avg: ${numObstaclesPerRow.sum.toDouble / numRows}")
    println(s"min: ${numObstaclesPerRow.min}")
    println(s"max: ${numObstaclesPerRow.max}")
    println(s"median: ${numObstaclesPerRow.sorted()(numRows / 2)}")
    println(s"90th percentile: ${numObstaclesPerRow.sorted()(numRows * 9 / 10)}")
  }

  def printPatrolPathsWithNewObstacles(
    obstacleCoords: Set[(Int, Int)],
    visited: Set[((Int, Int), (Int, Int))],
    turns: Set[((Int, Int), (Int, Int))],
    grid: Vector[Vector[Char]],
  ): Unit = {
    val gridWithNewObstacles = obstacleCoords.foldLeft(grid) { case (grid, (x, y)) => grid.updated(x, grid(x).updated(y, 'O')) }

    val gridWithPatrolPath = visited.foldLeft(gridWithNewObstacles) { case (grid, (pos@(x, y), dir)) =>
      // TODO: quite hacky
      val mark = if (visited.contains(pos -> calcLeftTurnBasedOnCurrentFacing(dir)) || visited.contains(pos -> calcRightTurnBasedOnCurrentFacing(dir)) || turns.map(_._1).contains(pos))
        '+'
      else if (dir == up || dir == down)
        '|'
      else
        '-'
      grid.updated(x, grid(x).updated(y, mark))
    }

    println(gridWithNewObstacles.map(_.mkString).mkString("\n"))
    println("")
    println(gridWithPatrolPath.map(_.mkString).mkString("\n"))

    obstacleCoords.foreach { case (x, y) =>
      val pathWithObstacle = gridWithPatrolPath.updated(x, gridWithPatrolPath(x).updated(y, 'O'))
      println(pathWithObstacle.map(_.mkString).mkString("\n"))
      println("")
    }
  }

  def task2(): Int = {
    val (startingCoords@(startX, startY), grid) = readInput()

    val newObstacles = moveTillCanWithNewObstacleSimulation(startingCoords, grid)

    val gridWithNewObstacles = newObstacles
      .foldLeft(grid) { case (grid, (x, y)) => grid.updated(x, grid(x).updated(y, 'O')) }
    val gridWithNewObstaclesAndStartingCoords =  gridWithNewObstacles.updated(startX, gridWithNewObstacles(startX).updated(startY, '^'))
    println(gridWithNewObstacles.map(_.mkString).mkString("\n"))

    newObstacles.size
  }
}
