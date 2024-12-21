package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.*
import adventofcode2024.common.graphs.Dijkstra.Move

import scala.annotation.{nowarn, tailrec}

object Day20 {
  def main(args: Array[String]): Unit = {
//    println(task1())
    println(task2())
  }

  enum TrackObject {
    case Wall, Empty, Start, End

    def pretty: String = this match {
      case Wall => "#"
      case Empty => "."
      case Start => "S"
      case End => "E"
    }
  }
  import TrackObject.*

  type RaceTrack = Vector[Vector[TrackObject]]

  def readInput(path: String): Vector[Vector[TrackObject]] = {
    val bufferedSource = io.Source.fromResource(path)
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines.map { line =>
      line.toVector.map {
        case '#' => Wall
        case '.' => Empty
        case 'S' => Start
        case 'E' => End
      }
    }
  }

  def findStart(track: RaceTrack): Coords = {
    track.findElem(_ == Start).get
  }

  def calcNextMoves(track: RaceTrack, prevMove: Move[Coords]): Set[Move[Coords]] = {
    val progressingNeighbours = track.neighboursOf(prevMove.to)
      .filter { pos =>
        track.at(pos).exists((obj: TrackObject) => obj != Wall)
      } - prevMove.to - prevMove.from

    progressingNeighbours.map { nextPos =>
      Move(from = prevMove.to, to = nextPos, cost = prevMove.cost + 1)
    }
  }

  def calcWallsToDelete(track: RaceTrack): Set[Coords] =
    Coords
      .rangeInclusive(Coords(1, 1), Coords(track.size - 2, track.head.size - 2))
      .filter(pos => track.at(pos).contains(Wall))
      .toSet

  def deleteWall(track: RaceTrack, wallPos: Coords): RaceTrack = track.updated(wallPos, Empty)

  def prettyTrack(track: RaceTrack): String = {
    track.map(_.map(_.pretty).mkString).mkString("\n")
  }

  case class Cheat(
    deletedWallPos: Coords,
    totalTime: Long,
  )

  def findCheats(originalTrack: RaceTrack, calcBestRouteCost: RaceTrack => Long): Set[Cheat] = {
    calcWallsToDelete(originalTrack).map { wallPos =>
      val newTrack = deleteWall(originalTrack, wallPos)
      val cost = calcBestRouteCost(newTrack)
      Cheat(wallPos, cost)
    }
  }

  def calcOgPathDistances(originalTrack: RaceTrack): Map[Coords, Int] = {
    val start = findStart(originalTrack)

    val originalPath = Dijkstra.exploreSingleOptimalRoute(
      graph = originalTrack,
      start = start,
      isEnd = (pos: Coords) => originalTrack.at(pos).contains(End),
      nextMoves = calcNextMoves
    ).get.traceBackSingleOptimalPath.get

    (start +: originalPath).zipWithIndex.toMap
  }

  case class ProCheat(
    from: Coords,
    to: Coords,
    saving: Long,
  )

  def calcPossibleCheatsFrom(track: RaceTrack, ogPathDistances: Map[Coords, Int], from: Coords): Set[ProCheat] = {
    val maxDistance = 20
    val possibleCheatDestinations = Coords
      .rangeInclusive(from - Coords(maxDistance, maxDistance), from + Coords(maxDistance, maxDistance))
      .filter(pos => pos.isWithinBounds(track.size, track.head.size))
      .filter(pos => Coords.manhattanDist(from, pos) <= maxDistance)
      .filter(pos => track.at(pos).exists(_ != Wall))

    possibleCheatDestinations.map { cheatDest =>
      val cheatDistance = Coords.manhattanDist(from, cheatDest)
      val saving = ogPathDistances(cheatDest) - (ogPathDistances(from) + cheatDistance) // TODO: safe?
      ProCheat(from, cheatDest, saving)
    }.filter(_.saving > 0).toSet
  }

  def calcAllPossibleCheats(track: RaceTrack): Set[ProCheat] = {
    val ogPathsDistances = calcOgPathDistances(track)

    ogPathsDistances.keySet.flatMap { pos =>
      calcPossibleCheatsFrom(track, ogPathsDistances, pos)
    }
  }

  def task1(): Int = {
    val originalTrack = readInput("day20.txt")
    val start = findStart(originalTrack)

    println(prettyTrack(originalTrack))
    println(start)
    println()

    def calcBestRouteCost(track: RaceTrack): Long =
      Dijkstra.exploreSingleOptimalRoute(
        graph = track,
        start = start,
        isEnd = (pos: Coords) => originalTrack.at(pos).contains(End),
        nextMoves = calcNextMoves
      ).get.cost

    val originalCost = calcBestRouteCost(originalTrack)
    val cheats = findCheats(originalTrack, calcBestRouteCost)
    println(s"cost: ${originalCost}")

    val cheatsByTimeSaved = cheats
      .toVector
      .map(cheat => (originalCost - cheat.totalTime) -> cheat)
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .toMap
    val numCheatsBySavings = cheatsByTimeSaved
      .toVector.map { case (saving, cheats) => saving -> cheats.size }
      .sortBy(_._1)
    println(numCheatsBySavings)

    numCheatsBySavings.collect { case (saving, numCheats) if saving >= 100 => numCheats }.sum
  }

  def task2(): Int = {
    val originalTrack = readInput("day20.txt")
    val start = findStart(originalTrack)

    println(prettyTrack(originalTrack))
    println(start)
    println()

    val numCheatsBySavings = calcAllPossibleCheats(originalTrack)
      .toVector
      .groupBy(_.saving)
      .view
      .mapValues(_.size)
      .toMap

    val numCheatsBySavingsOrdered = numCheatsBySavings
      .toVector
      .filter { case (saving, _) => saving >= 50 }
      .sortBy { case (saving, _) => saving }

//    assert(numCheatsBySavingsOrdered == Vector((50,32), (52,31), (54,29), (56,39), (58,25), (60,23), (62,20), (64,19), (66,12), (68,14), (70,12), (72,22), (74,4), (76,3)))
    println(numCheatsBySavingsOrdered)

    numCheatsBySavingsOrdered
      .collect { case (saving, numCheats) if saving >= 100 => numCheats }
      .sum
  }
}

// 2411064 HI
