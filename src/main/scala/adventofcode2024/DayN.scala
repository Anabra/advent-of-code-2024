package adventofcode2024

import adventofcode2024.common._
import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object DayN {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  def readInput(): Vector[Int] = {
    val bufferedSource = io.Source.fromResource("dayN.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    Vector()
  }

  def task1(): Int = {
    val fsIndex = readInput()

    42
  }

  def task2(): Int = {
    val fsIndex = readInput()
    42
  }
}
