package adventofcode2024

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import adventofcode2024.common.{Coords, iterateWhile}
import adventofcode2024.common.VectorVectorExtensions._

object Day15 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  type WarehouseMap = Vector[Vector[Char]]

  case class Input(
    warehouse: WarehouseMap,
    moves: Vector[Coords],
  )

  def readInput(): Input = {
    val bufferedSource = io.Source.fromResource("day15_small.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val (mapLines, _ +: movesLines) = lines.span(_.nonEmpty)

    val map = mapLines.map(_.toVector)
    val moves = movesLines.mkString.toVector.map {
      case '^' => Coords(-1, 0)
      case 'v' => Coords(1, 0)
      case '>' => Coords(0, 1)
      case '<' => Coords(0, -1)
    }

    Input(map, moves)
  }

  def findRobot(map: WarehouseMap): Coords = {
    val x = map.indexWhere(_.contains('@'))
    val y = map(x).indexOf('@')
    Coords(x, y)
  }

  def prettyPrintWarehouse(map: WarehouseMap): Unit = {
    map.foreach { row =>
      println(row.mkString)
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
      if (pos.isWithinBounds(dimX, dimY) && warehouse(pos.x)(pos.y) == 'O') {
        Some(pos + move)
      } else {
        None
      }
    }
    val firstNonBoxCoords@Coords(fstNonBoxX, fstNonBoxY) = boxCoordsInDirection.last + move
    warehouse(fstNonBoxX)(fstNonBoxY) match {
      case '#' =>
        (robotPos, warehouse)
      case '.' =>
        val newWarehouse = warehouse 
          .updated(firstNonBoxCoords, 'O')
          .updated(firstBoxCoords, '@')
          .updated(robotPos, '.')
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
      case '#' => (robotPos, warehouse)
      case '.' => (newRobotPos, warehouse.updated(newRobotPos, '@').updated(robotPos, '.'))
      case 'O' => shiftBoxes(robotPos, warehouse, move)
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
