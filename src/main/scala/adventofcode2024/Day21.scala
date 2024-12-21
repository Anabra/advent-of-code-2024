package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.*

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import scala.util.{Left, Right}

object Day21 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  enum NumberpadKey {
    case Num(n: Int)
    case Activate
  }

  enum ArrowpadKey {
    case Up, Down, Left, Right
    case Activate

    def pretty: String = this match {
      case Up => "^"
      case Down => "v"
      case Left => "<"
      case Right => ">"
      case Activate => "A"
    }
  }

  case class State(
    numberpad: NumberpadKey,
    arrowpads: Vector[ArrowpadKey],
  )

  type Codes = Vector[Vector[NumberpadKey]]

  def readInput(path: String): Codes = {
    val bufferedSource = io.Source.fromResource(path)
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines.map { line =>
      line.toVector.map {
        case 'A' => NumberpadKey.Activate
        case key => NumberpadKey.Num(key.asDigit)
      }
    }
  }

  def task1(): Int = {
    val codes = readInput("day21.txt")
    println(codes)
    42
  }

  def task2(): Int = {
    val fsIndex = readInput("day21.txt")
    42
  }
}
