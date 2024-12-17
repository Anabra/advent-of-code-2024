package adventofcode2024

import adventofcode2024.Day16.Task1.Move
import adventofcode2024.common.*

import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable.Vector as curPos
import scala.collection.mutable

object Day16 {
  def main(args: Array[String]): Unit = {
//    println(Task1.run())
    println(Task2.run())
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
    val bufferedSource = io.Source.fromResource("day16.txt")
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
    val moves = routeWithoutStart.zip(routeWithoutStart.drop(1)).map { case (from, to) => Task1.Move(from, to, 0) }
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

  def prettyMazeWithCoords(maze: Maze, coords: Set[Coords]): String = {
    val mazeWithCoords = maze.zipWithIndex.map { case (row, rowIx) =>
      row.zipWithIndex.map { case (cell, colIx) =>
        if (coords.contains(Coords(rowIx, colIx))) {
          "O"
        } else {
          cell.pretty
        }
      }
    }
    mazeWithCoords.map(_.mkString).mkString("\n")
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

  object Task1 {
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

    def findOptimalRoute(startPos: Coords, endPos: Coords, maze: Maze): (Vector[Coords], Int) = {
      val initialMove = Move(startPos - Coords(0, 1), startPos, 0) // we start facing East
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
            todo.enqueue(newMoves.toVector *)
            loop(visited + (moveToProcess.to -> moveToProcess))
          }
        }
      }

      val visits = loop(Map.empty)
      val optimalRoute = tracebackRoute(maze, visits)
      val cost = visits(endPos).cost

      optimalRoute -> cost
    }

    def run(): Int = {
      val maze = readInput()
      println(prettyMaze(maze))

      val (startPos, endPos) = findStartEnd(maze)
      val (optimalRoute, cost) = findOptimalRoute(startPos, endPos, maze)

      println()
      println(prettyMazeWithRoute(maze, optimalRoute))

      cost
    }
  }

  object Task2 {
    case class State(
      pos: Coords,
      dir: Coords,
    )

    case class Move(
      from: State,
      to: State,
      cost: Int,
    ) {
      def direction: Coords = to.pos - from.pos
    }

    def calcProgressingMoves(maze: Maze, lastMove: Move): Set[Move] = {
      val progressingDirections = Set(Coords(0, 1), Coords(0, -1), Coords(1, 0), Coords(-1, 0)) - lastMove.direction.inverse
      progressingDirections
        .map(dir => (dir, State(lastMove.to.pos + dir, dir)))
        .filter { case (_, State(nextPos, _)) => nextPos.isWithinBounds(maze.size, maze.head.size) && maze(nextPos.x)(nextPos.y) != MazeObject.Wall }
        .map { case (dir, nextState) =>
          val additionalCost = if (dir == lastMove.direction) 1 else 1001
          Move(lastMove.to, nextState, lastMove.cost + additionalCost)
        }
    }

    def tracebackAllOptimalRoutes(maze: Maze, visits: Map[State, Set[Move]]): (Set[Coords], Int) = {
      val (startPos, endPos) = findStartEnd(maze)
      val allDirections = Set(Coords(0, 1), Coords(0, -1), Coords(1, 0), Coords(-1, 0))
      val allPossibleFinalStates = allDirections.map(dir => State(endPos, dir)).filter(st => visits.contains(st))
      val minCostToFinalState = allPossibleFinalStates.flatMap(st => visits(st).map(_.cost)).min
      val finalStatesWithOptimalRoutesLeadingToThem = allPossibleFinalStates.filter(st => visits(st).forall(_.cost == minCostToFinalState))

      val todo = mutable.Queue(finalStatesWithOptimalRoutesLeadingToThem.toSeq*)

      @tailrec
      def loop(visited: Set[State]): Set[Coords] = {
        if (todo.isEmpty) {
          visited.map(_.pos)
        } else {
          val stateToProcess = todo.dequeue()
          if (visited.contains(stateToProcess)) {
            loop(visited)
          } else {
            val prevStates = visits(stateToProcess).map(_.from)
            val newStates = prevStates.diff(visited)
            todo.enqueueAll(newStates)
            loop(visited + stateToProcess)
          }
        }
      }

      loop(Set.empty) -> minCostToFinalState
    }

    def findOptimalRoute(startPos: Coords, endPos: Coords, maze: Maze): (Set[Coords], Int) = {
      val fictionalPreStartingState = State(startPos - Coords(0, 1), Coords(0, 1))
      val startState = State(startPos, Coords(0, 1)) // we start facing East
      val initialMove = Move(fictionalPreStartingState, startState, 0)
      val todo = mutable.PriorityQueue(initialMove)(Ordering.by[Move, Int](_.cost).reverse)

      @tailrec
      def loop(visited: Map[State, Set[Move]]): Map[State, Set[Move]] = {
        if (todo.isEmpty) {
          visited
        } else {
          val moveToProcess = todo.dequeue()
          visited.get(moveToProcess.to) match {
            case Some(prevMoves) =>
              if (prevMoves.forall(moveToProcess.cost == _.cost)) {
                loop(visited.updated(moveToProcess.to, prevMoves + moveToProcess))
              } else if (prevMoves.exists(moveToProcess.cost < _.cost)) {
                throw new IllegalStateException("We should never reach this point")
              } else {
                loop(visited)
              }
            case None =>
              if (maze.at(moveToProcess.to.pos) == MazeObject.End) {
                visited + (moveToProcess.to -> Set(moveToProcess))
              } else {
                val newMoves = calcProgressingMoves(maze, moveToProcess)
                todo.enqueue(newMoves.toVector *)
                loop(visited + (moveToProcess.to -> Set(moveToProcess)))
              }
          }
        }
      }

      val visits = loop(Map.empty).updatedWith(startState) {
        case Some(moves) => Some(Set.empty)
        case None => throw new IllegalStateException("We should never reach this point")
      }
      tracebackAllOptimalRoutes(maze, visits)
    }

    def run(): (Int, Int) = {
      val maze = readInput()
      println(prettyMaze(maze))

      val (startPos, endPos) = findStartEnd(maze)
      val (coordsOnOptimalRoutes, cost) = findOptimalRoute(startPos, endPos, maze)

      println()
      println(prettyMazeWithCoords(maze, coordsOnOptimalRoutes))

      coordsOnOptimalRoutes.size -> cost
    }
  }

}


// 73400 too high
