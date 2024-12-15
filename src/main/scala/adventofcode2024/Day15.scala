package adventofcode2024

import adventofcode2024.Day15.WarehouseObject.Robot

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import adventofcode2024.common.{Coords, iterateWhile}
import adventofcode2024.common.VectorVectorExtensions.*

object Day15 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  enum WarehouseObject:
    case Wall, Empty, Box, Robot

  import WarehouseObject.*

  type WarehouseMap = Vector[Vector[WarehouseObject]]

  case class Input(
    warehouse: WarehouseMap,
    moves: Vector[Coords],
  )

  def readInput(): Input = {
    val bufferedSource = io.Source.fromResource("day15_small.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val (mapLines, _ +: movesLines) = lines.span(_.nonEmpty)

    val warehouse = mapLines.map { line =>
      line.toVector.map {
        case '.' => Empty
        case '#' => Wall
        case 'O' => Box
        case '@' => Robot
      }
    }

    val moves = movesLines.mkString.toVector.map {
      case '^' => Coords(-1, 0)
      case 'v' => Coords(1, 0)
      case '>' => Coords(0, 1)
      case '<' => Coords(0, -1)
    }

    Input(warehouse, moves)
  }

  def findRobot(map: WarehouseMap): Coords = {
    val x = map.indexWhere(_.contains(Robot))
    val y = map(x).indexOf(Robot)
    Coords(x, y)
  }

  def prettyPrintWarehouse(warehouse: WarehouseMap): Unit = {
    warehouse.foreach { row =>
      val prettyRow = row.map {
        case Wall  => '#'
        case Empty => '.'
        case Box   => 'O'
        case Robot => '@'
      }
      println(prettyRow.mkString)
    }
  }

  // assumes the map is surrounded by walls
  // assumes there is at least one box in the direction of the move
  def shiftBoxes(robotPos: Coords, warehouse: WarehouseMap, move: Coords): (Coords, WarehouseMap) = {
    val dimX = warehouse.size
    val dimY = warehouse.head.size
    val firstBoxCoords@Coords(fstBoxX, fstBoxY) = robotPos + move
    // could do this with an iterator
    val boxCoordsInDirection = iterateWhile(firstBoxCoords) { pos =>
      val newPos = pos + move
      if (newPos.isWithinBounds(dimX, dimY) && warehouse(newPos.x)(newPos.y) == Box) {
        Some(newPos)
      } else {
        None
      }
    }
    val firstNonBoxCoords@Coords(fstNonBoxX, fstNonBoxY) = boxCoordsInDirection.last + move
    warehouse(fstNonBoxX)(fstNonBoxY) match {
      case Wall =>
        (robotPos, warehouse)
      case Empty =>
        val newWarehouse = warehouse
          .updated(firstNonBoxCoords, Box)
          .updated(firstBoxCoords, Robot)
          .updated(robotPos, Empty)
        (firstBoxCoords, newWarehouse)
      case x =>
        throw new Exception(s"Unexpected object. coords: ${firstNonBoxCoords}, object: '${x}'")
    }
  }

  def simulate(robotPos: Coords, warehouse: WarehouseMap, move: Coords): (Coords, WarehouseMap) = {
    println(s"Robot pos: ${robotPos}, move: ${move}")
    prettyPrintWarehouse(warehouse)
    println("")
    val dimX = warehouse.size
    val dimY = warehouse.head.size
    val newRobotPos@Coords(newRX, newRY) = robotPos + move

    val newRobotsPosObject = warehouse(newRX)(newRY)
    // assuming the map is surrounded by walls
    newRobotsPosObject match {
      case Wall  => (robotPos, warehouse)
      case Empty => (newRobotPos, warehouse.updated(newRobotPos, Robot).updated(robotPos, Empty))
      case Box   => shiftBoxes(robotPos, warehouse, move)
    }
  }

  def task1(): Int = {
    val Input(warehouse, moves) = readInput()
    val robotPos = findRobot(warehouse)

    val (finalRobotPos, finalWarehouse) = moves.foldLeft(robotPos -> warehouse) { case ((curRobotPos, curWarehouse), move) =>
      simulate(curRobotPos, curWarehouse, move)
    }

    println(robotPos)
    println(moves)
    prettyPrintWarehouse(finalWarehouse)

    42
  }

  def task2(): Int = {
    val fsIndex = readInput()
    42
  }
}
