package adventofcode2024

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.mutable

object Day7 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  def readInput(): Vector[(Long, Vector[Int])] = {
    val bufferedSource = io.Source.fromResource("day7.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines.map { line =>
      val Array(rawResult, rawOperands) = line.split(":")
      rawResult.toLong -> rawOperands.trim.split(" ").map(_.toInt).toVector
    }
  }

  enum Operator:
    case Add, Multiply, Concat

  def calcPossibleOperators(expectedResult: Long, operands: Vector[Int]): Option[Vector[Operator]] = {
    val todo = mutable.Stack((expectedResult, operands.reverse, Vector.empty[Operator]))
    @tailrec
    def loop(): Option[Vector[Operator]] = {
      if (todo.isEmpty) {
        None
      } else {
        val (curResult, operands, operatorsSoFar) = todo.pop()
        operands match {
          case Vector(lastOperand) =>
            if (lastOperand == curResult) {
              Some(operatorsSoFar)
            } else {
              loop()
            }
          case curOperand +: earlierOperands =>
            if (curResult % curOperand == 0) {
              todo.push((curResult / curOperand, earlierOperands, operatorsSoFar :+ Operator.Multiply))
            }

            // no way to rule out addition
            todo.push((curResult - curOperand, earlierOperands, operatorsSoFar :+ Operator.Add))
            loop()
          case Vector() => throw new Exception("should never happen")
        }
      }
    }

    loop()
  }

  def task1(): Long = {
    val input = readInput()

    input.filter { case (expectedResult, operands) =>
      calcPossibleOperators(expectedResult, operands).isDefined
    }.map(_._1).sum
  }

  def calcPossibleOperatorsWithConcat(expectedResult: Long, operands: Vector[Int]): Option[Vector[Operator]] = {
    val todo = mutable.Stack((expectedResult, operands.reverse, Vector.empty[Operator]))
    @tailrec
    def loop(): Option[Vector[Operator]] = {
      if (todo.isEmpty) {
        None
      } else {
        val (curResult, operands, operatorsSoFar) = todo.pop()
        operands match {
          case Vector(lastOperand) =>
            if (lastOperand == curResult) {
              Some(operatorsSoFar)
            } else {
              loop()
            }
          case curOperand +: earlierOperands =>
            if (curResult % curOperand == 0) {
              todo.push((curResult / curOperand, earlierOperands, operatorsSoFar :+ Operator.Multiply))
            }

            val nonEmptyPrefixPattern = """(\d+)""".r
            val suffixPattern = s"""${nonEmptyPrefixPattern.regex}${curOperand}""".r
            curResult.toString match {
              case suffixPattern(nonEmptyPrefix) =>
                todo.push((nonEmptyPrefix.toLong, earlierOperands, operatorsSoFar :+ Operator.Concat))
              case _ => ()
            }

            // we can never rule out addition
            todo.push((curResult - curOperand, earlierOperands, operatorsSoFar :+ Operator.Add))

            loop()
          case Vector() => throw new Exception("should never happen")
        }
      }
    }

    loop()
  }

  def task2(): Long = {
    val input = readInput()

    input.filter { case (expectedResult, operands) =>
      calcPossibleOperatorsWithConcat(expectedResult, operands).isDefined
    }.map(_._1).sum
  }
}
