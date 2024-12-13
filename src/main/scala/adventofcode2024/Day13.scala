package adventofcode2024

import java.util.regex.Pattern
import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import scala.util.matching.Regex

object Day13 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  case class Input(
    a: (Long, Long),
    b: (Long, Long),
    t: (Long, Long),
  )

  def extractNumbers(pattern: Regex, line: String): (Long, Long) = line match {
      case pattern(a1, a2) => (a1.toLong, a2.toLong)
    }

  def readInput(): Vector[Input] = {
    val bufferedSource = io.Source.fromResource("day13.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val buttonPattern = """Button [AB]: X\+(\d+), Y\+(\d+)""".r
    val prizePattern = """Prize: X=(\d+), Y=(\d+)""".r

    lines.filter(_.nonEmpty).grouped(3).map { case Vector(a, b, t) =>
      Input(
        extractNumbers(buttonPattern, a),
        extractNumbers(buttonPattern, b),
        extractNumbers(prizePattern,  t),
      )
    }.toVector
  }

  def calcASolution(input: Input): Option[(Long, Long)] = {
    val (a1, a2) = input.a
    val (b1, b2) = input.b
    val (t1, t2) = input.t

    val n = (t1 * b2 - t2 * b1) / (a1 * b2 - a2 * b1)
    val m = (t1 - n * a1) / b1

    val t1Recalculated = n * a1 + m * b1
    val t2Recalculated = n * a2 + m * b2

    if (t1Recalculated != t1 || t2Recalculated != t2) {
      None
    } else {
      Some((n, m))
    }
  }

  def calcTokens(solution: (Long, Long)): Long = {
    val (n, m) = solution
    n * 3 + m
  }

  def task1(): Long = {
    val inputs = readInput()

//    inputs.foreach { case Input(a, b, t) =>
//      val solutionOpt = calcASolution(Input(a, b, t))
//
//      solutionOpt match {
//        case Some((n, m)) =>
//          println(s"n: ${n}")
//          println(s"m: ${m}")
//        case None =>
//          println("NOT SOLVABLE")
//      }
//      println("--------------")
//    }

    inputs.flatMap(calcASolution).map(calcTokens).sum
  }

  def task2(): Long = {
    val inputs = readInput()
    val extra = 10000000000000L
    val biiiiiiigInputs = inputs.map { case Input(a, b, (t1 ,t2)) => Input(a, b, (t1 + extra, t2 + extra)) }
    biiiiiiigInputs.flatMap(calcASolution).map(calcTokens).sum
  }
}
