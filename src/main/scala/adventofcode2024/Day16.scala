package adventofcode2024

import adventofcode2024.common._

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object Day16 {
  def main(args: Array[String]): Unit = {
    println(task1())
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
    val bufferedSource = io.Source.fromResource("day16_small.txt")
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

  def prettyMazeWithAllOptimalRouteTiles(maze: Maze, tilesOnOptimalRoutes: Set[Coords]): String = {
    val mazeWithoutRoute = maze.map(_.map(_.pretty))
    val mazeWithTiles = tilesOnOptimalRoutes.foldLeft(mazeWithoutRoute) { case (curMaze, curTile) =>
      curMaze.updated(curTile, "O")
    }
    mazeWithTiles.map(_.mkString).mkString("\n")
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

  def tracebackARoute(maze: Maze, visits: Map[Coords, Move]): Vector[Coords] = {
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

  def findAlLCoordsOnAnyOptimalPath(maze: Maze, visits: Map[Coords, Set[Move]]): Set[Coords] = {
    val (startPos, endPos) = findStartEnd(maze)
    val todo = mutable.Queue(endPos)

    @tailrec
    def loop(coordsOnOptimalPaths: Set[Coords]): Set[Coords] = {
      if (todo.isEmpty) {
        coordsOnOptimalPaths
      } else {
        val curPos = todo.dequeue()

        if (coordsOnOptimalPaths.contains(curPos) || curPos == startPos) {
          loop(coordsOnOptimalPaths)
        } else {
          val moves = visits(curPos)
          val minCost = moves.map(_.cost).min
          val nextPositionsWithMinCost = moves.filter(_.cost == minCost).map(_.from)
          todo.enqueueAll(nextPositionsWithMinCost)
          loop(coordsOnOptimalPaths + curPos)
        }
      }
    }

    loop(Set.empty)
  }

  type Direction = Coords

  def findOptimalRoute(startPos: Coords, endPos: Coords, maze: Maze): (Vector[Coords], Int, Set[Coords]) = {
    val preStartFictionalCoord = startPos - Coords(0,1) // to account for facing east
    val initialMove = Move(preStartFictionalCoord, startPos, 0)
    val todo = mutable.PriorityQueue(initialMove)(Ordering.by[Move, Int](_.cost).reverse)

    @tailrec
    def loop(visited: Map[(Coords), Set[Move]]): Map[Coords, Set[Move]] = {
      if (todo.isEmpty) {
        visited
      } else {
        val moveToProcess = todo.dequeue()
        visited.get(moveToProcess.to) match {
          case Some(prevVisit) =>
            loop(visited.updated(moveToProcess.to, prevVisit + moveToProcess))
          case None =>
            if (maze.at(moveToProcess.to) == MazeObject.End) {
              loop(visited.updated(moveToProcess.to, Set(moveToProcess)))
            } else {
              val newMoves = calcProgressingMoves(maze, moveToProcess)
              todo.enqueue(newMoves.toVector*)
              loop(visited + (moveToProcess.to -> Set(moveToProcess)))
            }
        }
      }
    }

    val visits = loop(Map.empty)
    val optimalRoute = tracebackARoute(maze, visits.view.mapValues(_.head).toMap)
    val cost = visits(endPos).head.cost

    val coordsOnAnyOptimalPaths = findAlLCoordsOnAnyOptimalPath(maze, visits)

    (optimalRoute, cost, coordsOnAnyOptimalPaths)
  }

  def task1(): (Int, Int) = {
    val maze = readInput()
    println(prettyMaze(maze))

    val (startPos, endPos) = findStartEnd(maze)
    val (optimalRoute, cost, tilesOnOptimalRoutes) = findOptimalRoute(startPos, endPos, maze)

    println()
    println(prettyMazeWithRoute(maze, optimalRoute))
    println()
    println(prettyMazeWithAllOptimalRouteTiles(maze, tilesOnOptimalRoutes))

    cost -> tilesOnOptimalRoutes.size
  }
}


// 73400 too high
