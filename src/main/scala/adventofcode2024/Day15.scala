package adventofcode2024

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import adventofcode2024.common.{Coords, iterateWhile, updated, at}

object Day15 {
  def main(args: Array[String]): Unit = {
    println(Task1.run())
    println(Task2.run())
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
      val bufferedSource = io.Source.fromResource("day15.txt")
      val lines = bufferedSource.getLines.toVector
      bufferedSource.close

      val (warehouseLines, _ +: movesLines) = lines.span(_.nonEmpty)

      val parsedWarehouse = warehouseLines.map { row =>
        row.toVector.map {
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

    @nowarn("msg=exhaustive")
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

      prettyPrintWarehouse(finalWarehouse)

      collectBoxCoords(finalWarehouse).map(calcGpsCoord).sum
    }
  }

  object Task2 {
    val boxInside = '⃞'

    val verticalBoxTop = '⎴'
    val verticalBoxBottom = '⎵'

    val horizontalBoxLeft = '['
    val horizontalBoxRight = ']'

    val normalBoxTopLeft = '┌'
    val normalBoxTopRight = '┐'
    val normalBoxBottomLeft = '└'
    val normalBoxBottomRight = '┘'
    val normalBoxHorizontalEdge = '─'
    val normalBoxVerticalEdge = '│'

    enum WarehouseObject:
      case Wall, Empty, Robot
      // This could be extended to n*m boxes
      // The internal coords describe the relative position of the box parts within the box
      // It'd be relatively easy to collect all coords for a box
      case BoxPart(dimX: Int, dimY: Int, relativeCoords: Coords)

    import WarehouseObject.*

    type WarehouseMap = Vector[Vector[WarehouseObject]]

    case class Input(
      warehouse: WarehouseMap,
      moves: Vector[Coords],
    )

    def readInput(): Input = {
      import Task1.{WarehouseObject => NarrowWarehouseObject}

      val Task1.Input(smallWarehouse, moves) = Task1.readInput()
      val wideWarehouse = smallWarehouse.map { row =>
        row.flatMap {
          case NarrowWarehouseObject.Wall  => Vector(WarehouseObject.Wall, WarehouseObject.Wall)
          case NarrowWarehouseObject.Empty => Vector(WarehouseObject.Empty, WarehouseObject.Empty)
          case NarrowWarehouseObject.Box   => Vector(WarehouseObject.BoxPart(1, 2, Coords(0,0)), WarehouseObject.BoxPart(1, 2, Coords(0,1)))
          case NarrowWarehouseObject.Robot => Vector(WarehouseObject.Robot, WarehouseObject.Empty)
        }
      }

      Input(wideWarehouse, moves)
    }

    // Pretty printing could become complex for n*m boxes, so we will hard-code it for the 1*2 case to match the visuals in the examples
    def prettyPrintWarehouse(warehouse: WarehouseMap): Unit = {
      warehouse.foreach { row =>
        val prettyRow = row.map {
          case Wall => '#'
          case Empty => '.'
          case BoxPart(1, 2, Coords(0,0)) => '['
          case BoxPart(1, 2, Coords(0,1)) => ']'
          case Robot => '@'
          case x => throw new Exception(s"Unexpected object: ${x}")
        }
        println(prettyRow.mkString)
      }
    }


    // TODO: maybe just keep topLeft and bottomRight, and add a method for generating the other two?
    case class BoxBoundingCoords(
      topLeft: Coords,
      topRight: Coords,
      bottomLeft: Coords,
      bottomRight: Coords,
    )

    def calcContactCoordsOfPushedBox(
      boxBoundingCoords: BoxBoundingCoords,
      move: Coords
    ): Vector[Coords] = {
      // we shift the original bounding box of the boxpart
      // if the resulting coord is still in the original bounding box, we discard it, it won't hit another box
      // this should leave exactly 2 coords
      // then we generate the range of all coords that could potentially come in contact with the box after the move
      val sortedMovedBoundingBoxCoords = Set(
        boxBoundingCoords.topLeft,
        boxBoundingCoords.topRight,
        boxBoundingCoords.bottomRight,
        boxBoundingCoords.bottomLeft,
      ).toVector
        .map(_ + move)
        .filterNot(_.isWithinBoundingBox(boxBoundingCoords.topLeft, boxBoundingCoords.bottomRight)) // this should leave exactly 2 coords
        .sortWith { (lhs, rhs) => lhs.x < rhs.x || (lhs.x == rhs.x && lhs.y < rhs.y) }

      // these could be the same
      val minCoord = sortedMovedBoundingBoxCoords.head
      val maxCoord = sortedMovedBoundingBoxCoords.last

      Coords.rangeInclusive(minCoord, maxCoord)
    }

    def calcBoxBoundingCoords(boxPart: BoxPart, boxPartAbsCoords: Coords): BoxBoundingCoords = {
      val boxOrigin = boxPartAbsCoords - boxPart.relativeCoords
      BoxBoundingCoords(
        topLeft = boxOrigin,
        topRight = boxOrigin + Coords(0, boxPart.dimY - 1),
        bottomLeft = boxOrigin + Coords(boxPart.dimX - 1, 0),
        bottomRight = boxOrigin + Coords(boxPart.dimX - 1, boxPart.dimY - 1),
      )
    }

    def collectBoxesToPush(
      ogHitBoxPart: BoxPart,
      ogHitBoxPartCoords: Coords,
      warehouse: WarehouseMap,
      move: Coords
    ): Set[BoxBoundingCoords] = {
      val todoContactCoords = mutable.Queue[Coords](ogHitBoxPartCoords)

      def loop(hitSoFar: Set[BoxBoundingCoords]): Set[BoxBoundingCoords] = {
        if (todoContactCoords.isEmpty) {
          hitSoFar
        } else {
          val curContactCoords = todoContactCoords.dequeue()
          if (hitSoFar.exists(boundingBox => curContactCoords.isWithinBoundingBox(boundingBox.topLeft, boundingBox.bottomRight))) {
            loop(hitSoFar)
          } else {
            val hitObject = warehouse.at(curContactCoords).get // it's safe, the warehouse is surrounded by walls
            hitObject match {
              // one of the boxes hit a wall, nothing moves
              case Wall => Set.empty
              // just continue
              case Empty => loop(hitSoFar)
              // we hit a box, we need to check if it indirectly hit other boxes
              case curHitBoxPart: BoxPart =>
                val hitBoxBoundingCoords = calcBoxBoundingCoords(curHitBoxPart, curContactCoords)
                val contactCoords = calcContactCoordsOfPushedBox(hitBoxBoundingCoords, move)
                todoContactCoords.enqueueAll(contactCoords)
                loop(hitSoFar + hitBoxBoundingCoords)
              case x =>
                throw new Exception(s"Unexpected object. coords: ${curContactCoords}, object: '${x}'")
            }
          }
        }
      }

      loop(Set.empty)
    }

    // assumes the map is surrounded by walls
    // assumes there is at least one box in the direction of the move
    def moveBoxes(
      boxBoundingBoxes: Set[BoxBoundingCoords],
      warehouse: WarehouseMap,
      move: Coords
    ): WarehouseMap = {
      val allHitBoxPartsWithAbsCoords = boxBoundingBoxes.toVector.flatMap { curBoundingBox =>
        val absCoords = Coords.rangeInclusive(curBoundingBox.topLeft, curBoundingBox.bottomRight)
        val boxMaxRelativeCoord = curBoundingBox.bottomRight - curBoundingBox.topLeft
        val relativeCoords = Coords.rangeInclusive(Coords(0,0), boxMaxRelativeCoord)
        relativeCoords.map(BoxPart(boxMaxRelativeCoord.x + 1, boxMaxRelativeCoord.y + 1, _)).zip(absCoords)
      }

      val warehouseWithoutHitBoxes = allHitBoxPartsWithAbsCoords
        .map(_._2)
        .foldLeft(warehouse) { case (curWarehouse, curCoords) =>
          curWarehouse.updated(curCoords, Empty)
        }

      val warehouseWithMovedBoxes = allHitBoxPartsWithAbsCoords
        .map { case (boxPart, absCoords) => (boxPart, absCoords + move) }
        .foldLeft(warehouseWithoutHitBoxes) { case (curWarehouse, (curBoxPart, curAbsCoords)) =>
          curWarehouse.updated(curAbsCoords, curBoxPart)
        }

      warehouseWithMovedBoxes
    }

    @nowarn("msg=exhaustive")
    def simulate(robotPos: Coords, warehouse: WarehouseMap, move: Coords): (Coords, WarehouseMap) = {
      val dimX = warehouse.size
      val dimY = warehouse.head.size
      val newRobotPos@Coords(newRX, newRY) = robotPos + move

      val newRobotsPosObject = warehouse(newRX)(newRY)
      // assuming the map is surrounded by walls
      newRobotsPosObject match {
        case Wall => (robotPos, warehouse)
        case Empty => (newRobotPos, warehouse.updated(newRobotPos, Robot).updated(robotPos, Empty))
        case hitBoxPart: BoxPart =>
          val boxesToPush = collectBoxesToPush(hitBoxPart, newRobotPos, warehouse, move)
          // mainly not to move the robot
          if (boxesToPush.isEmpty) {
            (robotPos, warehouse)
          } else {
            val newWarehouse = moveBoxes(boxesToPush, warehouse, move)
              .updated(newRobotPos, Robot)
              .updated(robotPos, Empty)
            (newRobotPos, newWarehouse)
          }
      }
    }

    def findRobot(map: WarehouseMap): Coords = {
      val x = map.indexWhere(_.contains(Robot))
      val y = map(x).indexOf(Robot)
      Coords(x, y)
    }

    def calcGpsCoords(warehouseMap: WarehouseMap)(boxOriginAbsCoords: Coords): Int =
      boxOriginAbsCoords.x * 100 + boxOriginAbsCoords.y

    def collectBoxOriginAbsCoords(warehouseMap: WarehouseMap): Vector[Coords] = {
      val dimX = warehouseMap.size
      val dimY = warehouseMap.head.size
      val coords = for {
        x <- 0 until dimX
        y <- 0 until dimY
        if warehouseMap(x)(y) match {
          case BoxPart(_, _, Coords(0,0)) => true
          case _ => false
        }
      } yield Coords(x, y)
      coords.toVector
    }

    def run(): Int = {
      val Input(warehouse, moves) = readInput()
      prettyPrintWarehouse(warehouse)
      println("")

      val robotPos = findRobot(warehouse)

      val (finalRobotPos, finalWarehouse) = moves.foldLeft(robotPos -> warehouse) { case ((curRobotPos, curWarehouse), move) =>
        val r = simulate(curRobotPos, curWarehouse, move)
//        prettyPrintWarehouse(r._2)
//        println()
        r
      }

      prettyPrintWarehouse(finalWarehouse)

      collectBoxOriginAbsCoords(finalWarehouse).map(calcGpsCoords(finalWarehouse)).sum
    }

  }
}
