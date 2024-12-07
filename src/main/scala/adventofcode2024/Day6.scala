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
    val bufferedSource = io.Source.fromResource("day6.txt")
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
      Some(
        grid(newX)(newY) match {
          case '.' =>
            ((newX, newY), direction)
          case '#' =>
            val relativeRightDirection = calcRightTurnBasedOnCurrentFacing(direction)
            val nextPos = calcNextPos(curPos, relativeRightDirection)
            (nextPos, relativeRightDirection)
        }
      )
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

  @tailrec
  def moveTillCanAdvanced(
    curPos: (Int, Int),
    curDirection: (Int, Int),
    grid: Vector[Vector[Char]],
    visited: Set[((Int, Int), (Int, Int))],
    turns: Set[((Int, Int), (Int, Int))]
  ): (Set[((Int, Int), (Int, Int))], Set[((Int, Int), (Int, Int))]) = {
    moveOnGrid(curPos, curDirection, grid) match {
      case None => (visited, turns)
      case Some((newPos, newDirection)) if newDirection == curDirection => moveTillCanAdvanced(
        newPos,
        newDirection,
        grid,
        visited + ((newPos, newDirection)),
        turns
      )
      case Some((newPos, newDirection)) if newDirection != curDirection => moveTillCanAdvanced(
        newPos,
        newDirection,
        grid,
        visited + ((newPos, newDirection)),
        turns + ((curPos, curDirection))
      )
    }
  }

  def calcLeftTurnBasedOnCurrentFacing(curFacing: (Int, Int)): (Int, Int) = curFacing match {
    case `up` => left
    case `left` => down
    case `down` => right
    case `right` => up
  }

  @tailrec
  def moveTillNextObstacle(
    curPos: (Int, Int),
    curDir: (Int, Int),
    grid: Vector[Vector[Char]],
    visited: Set[(Int, Int)]
  ): Set[(Int, Int)] = {
    moveOnGrid(curPos, curDir, grid) match {
      case None => visited
      case Some((_, newDir)) if newDir != curDir => visited
      case Some((newPos, newDir)) => moveTillNextObstacle(newPos, newDir, grid, visited + newPos)
    }
  }

  def reverseDirection(dir: (Int, Int)): (Int, Int) = dir match {
    case `up` => down
    case `right` => left
    case `down` => up
    case `left` => right
  }

  def calcPotentialLoopingTurnCoords(
    turnPos: (Int, Int),
    turnDir: (Int, Int),
    grid: Vector[Vector[Char]],
  ): Set[(Int, Int)] = {
    moveTillNextObstacle(turnPos, reverseDirection(turnDir), grid, Set.empty)
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
    val (startingCoords, grid) = readInput()

    printStats(grid)
    
    42
  }
}

