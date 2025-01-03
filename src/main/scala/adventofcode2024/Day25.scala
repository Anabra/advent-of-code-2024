package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.*

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object Day25 {
  def main(args: Array[String]): Unit = {
    println(task1())
  }

  type Key = Vector[Int]
  type Lock = Vector[Int]
  case class Input(
    keys: Vector[Key],
    locks: Vector[Lock],
  )

  def readInput(path: String): Input = {
    val bufferedSource = io.Source.fromResource(path)
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val (rawKeys, rawLocks) = lines
      .filter(_.nonEmpty)
      .grouped(7)
      .partition(lines => lines.head.toVector == Vector.fill(5)('.'))

    val keys = rawKeys.map { lines =>
      lines.tail.map(_.toVector).transpose.map(_.count(_ == '#'))
    }.toVector
    val locks = rawLocks.map { lines =>
      lines.tail.map(_.toVector).transpose.map(_.count(_ == '#'))
    }.toVector

    Input(keys, locks)
  }
  
  def itFits(key: Key, lock: Lock): Boolean =
    key.zip(lock).map { case (keyPin, lockPin) => keyPin + lockPin }.forall(_ <= 6)

  def task1(): Int = {
    val Input(keys, locks) = readInput("day25.txt")
    println(keys)
    println()
    println(locks)
    
    val fits = for {
      key <- keys
      lock <- locks
      if itFits(key, lock)
    } yield ()

    fits.size 
  }
}
