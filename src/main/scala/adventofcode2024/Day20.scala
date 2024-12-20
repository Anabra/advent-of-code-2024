package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.*
import adventofcode2024.common.graphs.Dijkstra.Move

import scala.annotation.{nowarn, tailrec}

object Day20 {
  def main(args: Array[String]): Unit = {
    println(task1())
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

  def prettyTrack(track: RaceTrack): String = {
    track.map(_.map(_.pretty).mkString).mkString("\n")
  }

  def task1(): Int = {
    val track = readInput("day20_small.txt")
    val start = findStart(track)

    println(prettyTrack(track))
    println(start)

    val shortestPathOpt = Dijkstra.exploreSingleOptimalRoute(
      graph = track,
      start = start,
      isEnd = (pos: Coords) => track.at(pos).contains(End),
      nextMoves = calcNextMoves
    )
    println(s"cost: ${shortestPathOpt.get.cost}")
    42
  }

  def task2(): Int = {
    val track = readInput("day20_small.txt")
    42
  }
}
