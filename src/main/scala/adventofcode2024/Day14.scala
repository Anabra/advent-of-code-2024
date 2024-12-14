package adventofcode2024

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

// NOTE: in this exercise, the x is the col index and y is the row index
object Day14 {
  def main(args: Array[String]): Unit = {
    tests()
    println(task1())
    println(task2())
  }

  case class Robot(
    startingPos: (Int, Int),
    velocity: (Int, Int),
  )

  def readInput(): Vector[Robot] = {
    val bufferedSource = io.Source.fromResource("day14.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val robotPattern = """p=(\d+),(\d+) v=(-?\d+),(-?\d+)""".r

    lines.map { case robotPattern(rawStartingPos1, rawStartingPos2, rawVelocity1, rawVeolocity2) =>
      Robot(
        (rawStartingPos1.toInt, rawStartingPos2.toInt),
        (rawVelocity1.toInt, rawVeolocity2.toInt),
      )
    }
  }

  def simulateRobot(dimX: Int, dimY: Int, time: Int)(robot: Robot): (Int, Int) = {
    val (x, y) = robot.startingPos
    val (vx, vy) = robot.velocity

    (math.floorMod(x + vx * time,  dimX), math.floorMod(y + vy * time, dimY))
  }

  case class Quadrant(
    fromInclusive: (Int, Int),
    toExclusive: (Int, Int),
  )

  def calcQuadrants(dimX: Int, dimY: Int): Vector[Quadrant] = {
    val xLimit = dimX / 2
    val yLimit = dimY / 2

    Vector(
      Quadrant((0,0), (xLimit, yLimit)),
      Quadrant((0, dimY - yLimit), (xLimit, dimY)),
      Quadrant((dimX - xLimit, dimY - yLimit), (dimX, dimY)),
      Quadrant((dimX - xLimit, 0), (dimX, yLimit)),
    )
  }

  def groupPositionsByQuadrants(dimX: Int, dimY: Int)(positions: Vector[(Int, Int)]): Vector[Vector[(Int, Int)]] = {
    val quadrants = calcQuadrants(dimX, dimY)

    val positionsWithQuadrant = positions.flatMap { case pos@(x, y) =>
      quadrants.find { case Quadrant((fromX, fromY), (toX, toY)) =>
        fromX <= x && x < toX && fromY <= y && y < toY
      }.map(_ -> pos)
    }

    positionsWithQuadrant
      .groupBy { case (quadrant, _) => quadrant}
      .view
      .mapValues(_.map { case (_, pos) => pos})
      .values
      .toVector
  }

  def calcSafetyScore(dimX: Int, dimY: Int, time: Int)(robots: Vector[Robot]): Int = {
    val robotFinalPositions = robots.map(simulateRobot(dimX, dimY, time))
    val positionsByQuadrants = groupPositionsByQuadrants(dimX, dimY)(robotFinalPositions)

    positionsByQuadrants.map(_.size).product
  }

  def tests(): Unit = {
    val (smallX, smallY) = (11, 7)

    assert(simulateRobot(smallX, smallY, 3)(Robot((0,0), (1,0))) == (3,0))
    assert(simulateRobot(smallX, smallY, 10)(Robot((0,0), (1,0))) == (10,0))
    assert(simulateRobot(smallX, smallY, 11)(Robot((0,0), (1,0))) == (0,0))

    assert(simulateRobot(smallX, smallY, 3)(Robot((0,0), (0,1))) == (0,3))
    assert(simulateRobot(smallX, smallY, 6)(Robot((0,0), (0,1))) == (0,6))
    assert(simulateRobot(smallX, smallY, 7)(Robot((0,0), (0,1))) == (0,0))

    assert(simulateRobot(smallX, smallY, 3)(Robot((5,5), (-1,0))) == (2,5))
    assert(simulateRobot(smallX, smallY, 5)(Robot((5,5), (-1,0))) == (0,5))
    assert(simulateRobot(smallX, smallY, 6)(Robot((5,5), (-1,0))) == (10,5))

    assert(simulateRobot(smallX, smallY, 3)(Robot((5,5), (0, -1))) == (5,2))
    assert(simulateRobot(smallX, smallY, 5)(Robot((5,5), (0, -1))) == (5,0))
    assert(simulateRobot(smallX, smallY, 6)(Robot((5,5), (0, -1))) == (5,6))
  }

  def task1(): Int = {
    val (smallX, smallY) = (101, 103)
    val robots =readInput()
    val time = 100

    calcSafetyScore(smallX, smallY, time)(robots)
  }

  def task2(): Int = {
    val robots = readInput()
    42
  }
}
