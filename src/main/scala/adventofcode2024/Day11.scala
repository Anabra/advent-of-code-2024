package adventofcode2024

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object Day11 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  def readInput(): Vector[Long] = {
    val bufferedSource = io.Source.fromResource("day11.txt")
    val line = bufferedSource.getLines.toVector.head
    bufferedSource.close

    line.split(" ").map(_.toLong).toVector
  }

  def numberLength(number: Long): Int = math.log10(number).toInt + 1

  def doSpaceTimeMutationMagic(number: Long): Vector[Long] = {
    val numLength = numberLength(number)

    if (number == 0) {
      Vector(1)
    } else if (numLength % 2 == 0) {
      val numberHalfLength = numLength / 2
      val tenPowa = math.pow(10, numberHalfLength).toLong

      val firstHalf = number / tenPowa
      val secondHalf = number % tenPowa
      Vector(firstHalf, secondHalf)
    } else {
      Vector(number * 2024)
    }
  }

  @tailrec
  def doItNTimesSlow(numbers: Vector[Long], times: Int): Vector[Long] = {
   if (times == 0) {
     numbers
   } else {
     val newNumbers = numbers.flatMap(doSpaceTimeMutationMagic)
     doItNTimesSlow(newNumbers, times - 1)
   }
  }

  @tailrec
  def doItNTimesTurboBoost(numbers: Map[Long, Long], times: Int): Map[Long, Long] = {
    if (times == 0) {
      numbers
    } else {
      val mutNumbers = mutable.Map[Long, Long]()
      numbers.foreach { case (number, numOccurrences) =>
        val newNumbers = doSpaceTimeMutationMagic(number)
        newNumbers.foreach { newNumber =>
          mutNumbers.updateWith(newNumber) {
            case Some(occurrences) => Some(occurrences + numOccurrences)
            case None => Some(numOccurrences)
          }
        }
      }

      doItNTimesTurboBoost(mutNumbers.toMap, times - 1)
    }
  }

  def task1(): Int = {
    val numbers = readInput()
    val result = doItNTimesSlow(numbers, 25)

//    println(result)
    result.size
  }

  def task2(): Long = {
    val numbers = readInput()
    val numbersMap = numbers.groupBy(identity).view.mapValues(_.size.toLong).toMap

    val result = doItNTimesTurboBoost(numbersMap, 75)

    result.values.sum
  }
}
