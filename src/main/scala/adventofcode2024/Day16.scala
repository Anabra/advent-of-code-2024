package adventofcode2024

import adventofcode2024.common._

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object Day16 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  enum MazeObject {
    case Wall, Empty, Start, End

    def pretty: String = this match {
      case MazeObject.Wall => "#"
      case MazeObject.Empty => "."
      case MazeObject.Start => "S"
      case MazeObject.End => "E"
    }
  }

  import MazeObject.*

  type Maze = Vector[Vector[MazeObject]]

  def readInput(): Maze = {
    val bufferedSource = io.Source.fromResource("day16_medium.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines.map(_.toVector.map {
      case '#' => MazeObject.Wall
      case '.' => MazeObject.Empty
      case 'S' => MazeObject.Start
      case 'E' => MazeObject.End
    })
  }

  def prettyMaze(maze: Maze): String = {
    maze.map(_.map(_.pretty).mkString).mkString("\n")
  }

  def prettyMazeWithRoute(maze: Maze, route: Vector[Coords]): String = {
    val mazeWithoutRoute = maze.map(_.map(_.pretty))
    val routeWithoutStart = route.drop(1)
    val moves = routeWithoutStart.zip(routeWithoutStart.drop(1)).map { case (from, to) => Move(from, to, 0) }
    val mazeWithMoves = moves.foldLeft(mazeWithoutRoute) { case (curMaze, curMove) =>
      curMove.direction match {
        case Coords(0,  1) => curMaze.updated(curMove.from, ">")
        case Coords(0, -1) => curMaze.updated(curMove.from, "<")
        case Coords(1,  0) => curMaze.updated(curMove.from, "v")
        case Coords(-1, 0) => curMaze.updated(curMove.from, "^")
      }
    }
    mazeWithMoves.map(_.mkString).mkString("\n")
  }

  def findStartEnd(maze: Maze): (Coords, Coords) = {
    val Vector(start, end) = maze.zipWithIndex.flatMap { case (row, rowIx) =>
      row.zipWithIndex.flatMap { case (cell, colIx) =>
        if (cell == MazeObject.Start || cell == MazeObject.End) {
          Some(cell, Coords(rowIx, colIx))
        } else {
          None
        }
      }
    }.sortBy { case (cell, _) => if (cell == MazeObject.Start) 0 else 1 }
      .map { case (_, coords) => coords }

    start -> end
  }

  case class Move(
    from: Coords,
    to: Coords,
    cost: Int,
  ) {
    def direction: Coords = to - from
  }

  def calcProgressingMoves(maze: Maze, lastMove: Move): Set[Move] = {
    val progressingDirections = Set(Coords(0, 1), Coords(0, -1), Coords(1, 0), Coords(-1, 0)) - lastMove.direction.inverse
    progressingDirections
      .map(dir => (dir, lastMove.to + dir))
      .filter { case (_, dest) => dest.isWithinBounds(maze.size, maze.head.size) && maze(dest.x)(dest.y) != MazeObject.Wall }
      .map { case (dir, dest) =>
        val additionalCost = if (dir == lastMove.direction) 1 else 1001
        Move(lastMove.to, lastMove.to + dir, lastMove.cost + additionalCost)
      }
  }

  def tracebackRoute(maze: Maze, visits: Map[Coords, Move]): Vector[Coords] = {
    val (startPos, endPos) = findStartEnd(maze)

    @tailrec
    def loop(curPos: Coords, routeSoFar: Vector[Coords]): Vector[Coords] = {
      if (curPos == startPos) {
        (curPos +: routeSoFar)
      } else {
        val move = visits(curPos)
        loop(move.from, curPos +: routeSoFar)
      }
    }

    loop(endPos, Vector.empty)
  }

  def findCheapestRoute(startPos: Coords, endPos: Coords, maze: Maze): (Vector[Coords], Int) = {
    val initialMove = Move(startPos - Coords(0,1), startPos, 0) // we start facing East
    val todo = mutable.PriorityQueue(initialMove)(Ordering.by[Move, Int](_.cost).reverse)

    @tailrec
    def loop(visited: Map[Coords, Move]): Map[Coords, Move] = {
      if (todo.isEmpty) {
        visited
      } else {
        val moveToProcess = todo.dequeue()
        if (visited.contains(moveToProcess.to)) {
          // no need to check here, Dijkstra guarantees that if we visited a node, it was on the optimal path
          loop(visited)
        } else if (maze.at(moveToProcess.to) == MazeObject.End) {
          visited + (moveToProcess.to -> moveToProcess)
        } else {
          val newMoves = calcProgressingMoves(maze, moveToProcess)
          todo.enqueue(newMoves.toVector*)
          loop(visited + (moveToProcess.to -> moveToProcess))
        }
      }
    }

    val visits = loop(Map.empty)
    val optimalRoute = tracebackRoute(maze, visits)
    val cost = visits(endPos).cost

    optimalRoute -> cost
  }

  def task1(): Int = {
    val maze = readInput()
    println(prettyMaze(maze))

    val (startPos, endPos) = findStartEnd(maze)
    val (optimalRoute, cost) = findCheapestRoute(startPos, endPos, maze)

    println()
    println(prettyMazeWithRoute(maze, optimalRoute))

    cost
  }

  def task2(): Int = {
    val fsIndex = readInput()
    42
  }
}


// 73400 too high
