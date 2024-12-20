package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.*
import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object DayN {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  def readInput(path: String): Vector[Int] = {
    val bufferedSource = io.Source.fromResource(path)
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    Vector()
  }

  def task1(): Int = {
    val fsIndex = readInput("dayN.txt")

    42
  }

  def task2(): Int = {
    val fsIndex = readInput("dayN.txt")
    42
  }
}
