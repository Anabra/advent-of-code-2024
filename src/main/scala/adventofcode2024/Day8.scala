package adventofcode2024

import scala.annotation.{nowarn, tailrec}
import adventofcode2024.common.iterateWhile

object Day8 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  type AntennaType = Char

  case class AntennaMap(
    dimX: Int,
    dimY: Int,
    antennaCoordsByFreq: Map[AntennaType, Set[(Int, Int)]]
  )

  def readInput(): AntennaMap = {
    val bufferedSource = io.Source.fromResource("day8.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val antennas = lines.zipWithIndex.flatMap { case (line, rowIx) =>
      line.zipWithIndex.collect { case (antennaType, colIx) if antennaType != '.' =>
        antennaType -> (rowIx, colIx)
      }
    }.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap

    AntennaMap(lines.size, lines.head.size, antennas)
  }

  def calcPairAntinodes(dimX: Int, dimY: Int, antenna1: (Int, Int), antenna2: (Int, Int)): Set[(Int, Int)] = {
    val (x1, y1) = antenna1
    val (x2, y2) = antenna2

    val (dx, dy) = (x2 - x1, y2 - y1)
    Set(
      (x1 - dx, y1 - dy),
      (x2 + dx, y2 + dy),
    ).filter { case (x, y) =>
      0 <= x && x < dimX && 0 <= y && y < dimY
    }
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) {
      Seq(a.abs,b.abs).max
    } else {
      gcd(b, a % b)
    }
  }

  def shift(
    dimX: Int, dimY: Int, dir: (Int, Int))(
    pos: (Int, Int),
  ): Option[(Int, Int)] = {
    val (x, y) = pos
    val (dx, dy) = dir
    val (newX, newY) = (x + dx, y + dy)
    if (0 <= newX && newX < dimX && 0 <= newY && newY < dimY) {
      Some(newX, newY)
    } else {
      None
    }
  }

  def calcPairAntinodesWithInfDistance(dimX: Int, dimY: Int, antenna1: (Int, Int), antenna2: (Int, Int)): Set[(Int, Int)] = {
    val (x1, y1) = antenna1
    val (x2, y2) = antenna2

    val (dx, dy) = (x2 - x1, y2 - y1)
    val diffGcd = gcd(dx, dy)
    val normDiff@(normDx, normDy) = (dx / diffGcd, dy / diffGcd)
    val normDiffOpposite = (-normDx, -normDy)

    val forwardCoords = iterateWhile((x1, y1))(shift(dimX, dimY, normDiff))
    val backwardCoords = iterateWhile((x1, y1))(shift(dimX, dimY, normDiffOpposite))

    (forwardCoords ++ backwardCoords ++ Seq(antenna1, antenna2)).toSet
  }

  def calcAllAntinodes(
    antennaMap: AntennaMap,
    calcPairAntinodes: (Int, Int, (Int, Int), (Int, Int)) => Set[(Int, Int)]
  ): Set[(Int, Int)] = {
    val AntennaMap(dimX, dimY, antennas) = antennaMap
    antennas.values.toSet.flatMap { curAntennaGroupCoords =>
      @nowarn("msg=exhaustive")
      val antennaCoordPairs = curAntennaGroupCoords.subsets(2).toSet.map { twoCoords =>
        twoCoords.toVector match {
          case Vector(antenna1, antenna2) => (antenna1, antenna2)
        }
      }
      antennaCoordPairs.flatMap { case (antenna1, antenna2) =>
        calcPairAntinodes(dimX, dimY, antenna1, antenna2)
      }
    }
  }

  def task1(): Int = {
    val antennaMap = readInput()
    calcAllAntinodes(antennaMap, calcPairAntinodes).size
  }

  def task2(): Int = {
    val antennaMap = readInput()
    calcAllAntinodes(antennaMap, calcPairAntinodesWithInfDistance).size
  }
}
/*
..A
...
.A.
...
A..

...A
..A.
.A..
A...

..A
...
A..

...A
....
..A.
....
.A..
....
A...

gcd(a, b) = gcd(b, a % b)

proof:
g = gcd(a, b)
it's a common divisor, so g | a and g | b
since b = a * m + a % b, then g | a % b
so the maximum number g that divides b and a % b will be the gcd(a, b)


 */
