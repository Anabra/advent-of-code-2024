package adventofcode2024

import adventofcode2024.common.*

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

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

  def prettyTrack(track: RaceTrack): String = {
    track.map(_.map(_.pretty).mkString).mkString("\n")
  }

  def task1(): Int = {
    val track = readInput("day20_small.txt")
    println(prettyTrack(track))
    42
  }

  def task2(): Int = {
    val track = readInput("day20_small.txt")
    42
  }
}
