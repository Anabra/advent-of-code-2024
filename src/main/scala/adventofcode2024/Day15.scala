package adventofcode2024

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import adventofcode2024.common.{Coords, iterateWhile}
import adventofcode2024.common.VectorVectorExtensions.*

object Day15 {
  def main(args: Array[String]): Unit = {
    println(Task1.run())
    println(Task2.run())
  }

  def readPreparsedInput(): (Vector[Vector[Char]], Vector[Coords]) = {
    val bufferedSource = io.Source.fromResource("day15.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val (warehouseLines, _ +: movesLines) = lines.span(_.nonEmpty)
    val moves = movesLines.mkString.toVector.map {
      case '^' => Coords(-1, 0)
      case 'v' => Coords(1, 0)
      case '>' => Coords(0, 1)
      case '<' => Coords(0, -1)
    }

    (warehouseLines.map(_.toVector), moves)
  }

  object Task1 {
    enum WarehouseObject:
      case Wall, Empty, Box, Robot

    import WarehouseObject.*

    type WarehouseMap = Vector[Vector[WarehouseObject]]

    case class Input(
      warehouse: WarehouseMap,
      moves: Vector[Coords],
    )

    def readInput(): Input = {
      val (warehouse, moves) = readPreparsedInput()

      val parsedWarehouse = warehouse.map { row =>
        row.map {
          case '.' => Empty
          case '#' => Wall
          case 'O' => Box
          case '@' => Robot
        }
      }

      Input(parsedWarehouse, moves)
    }

    def findRobot(map: WarehouseMap): Coords = {
      val x = map.indexWhere(_.contains(Robot))
      val y = map(x).indexOf(Robot)
      Coords(x, y)
    }

    def prettyPrintWarehouse(warehouse: WarehouseMap): Unit = {
      warehouse.foreach { row =>
        val prettyRow = row.map {
          case Wall => '#'
          case Empty => '.'
          case Box => 'O'
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
      val dimX = warehouse.size
      val dimY = warehouse.head.size
      val newRobotPos@Coords(newRX, newRY) = robotPos + move

      val newRobotsPosObject = warehouse(newRX)(newRY)
      // assuming the map is surrounded by walls
      newRobotsPosObject match {
        case Wall => (robotPos, warehouse)
        case Empty => (newRobotPos, warehouse.updated(newRobotPos, Robot).updated(robotPos, Empty))
        case Box => shiftBoxes(robotPos, warehouse, move)
      }
    }

    def calcGpsCoord(coords: Coords): Int = {
      coords.x * 100 + coords.y
    }

    def collectBoxCoords(warehouse: WarehouseMap): Vector[Coords] = {
      val dimX = warehouse.size
      val dimY = warehouse.head.size
      val coords = for {
        x <- 0 until dimX
        y <- 0 until dimY
        if warehouse(x)(y) == Box
      } yield Coords(x, y)
      coords.toVector
    }

    def run(): Int = {
      val Input(warehouse, moves) = readInput()
      val robotPos = findRobot(warehouse)

      val (finalRobotPos, finalWarehouse) = moves.foldLeft(robotPos -> warehouse) { case ((curRobotPos, curWarehouse), move) =>
        simulate(curRobotPos, curWarehouse, move)
      }

      println(robotPos)
      println(moves)
      prettyPrintWarehouse(finalWarehouse)

      collectBoxCoords(finalWarehouse).map(calcGpsCoord).sum
    }
  }

  object Task2 {
    def run(): Int = {
      val fsIndex = readPreparsedInput()
      42
    }

  }
}
