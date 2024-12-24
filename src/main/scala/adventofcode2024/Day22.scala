package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.*

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object Day22 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  def readInput(path: String): Vector[Long] = {
    val bufferedSource = io.Source.fromResource(path)
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines.map(_.toLong)
  }

  def mix(secretNum: Long, otherNum: Long): Long = secretNum ^ otherNum
  def prune(secretNum: Long): Long = secretNum % 16777216L

  def calcNext(secretNum: Long): Long = {
    val step1Res = prune(mix(secretNum, secretNum * 64L))
    val step2Res = prune(mix(step1Res, step1Res / 32L))
    val step3Res = prune(mix(step2Res, step2Res * 2048L))

    step3Res
  }

  case class PriceWithChanges(
    price: Long,
    changes: Vector[Long],
  )

  case class Input(
    prices: Vector[Long],
    changes: Vector[Long],
  ) {
    lazy val pricesWithChanges: Vector[PriceWithChanges] = {
      val changeSeqs = changes.sliding(4)
      prices.drop(4).zip(changeSeqs).map(PriceWithChanges.apply)
    }
  }

  def calcPricesWithChanges(initialSecretNum: Long): Input = {
    val secretNums = Vector.iterate(initialSecretNum, 2000 + 1)(calcNext)
    val prices = secretNums.map(_ % 10)
    val changes = prices.zip(prices.drop(1)).map { case (prev, next) => next - prev }
    Input(prices, changes)
  }

  def findFirstDeal(merchantDeals: Vector[PriceWithChanges], changeSeq: Vector[Long]): Option[Long] =
    merchantDeals.find(_.changes == changeSeq).map(_.price)

  def calcTotalPrice(allMerchantDeals: Vector[Vector[PriceWithChanges]], changeSeq: Vector[Long]): Long = {
    allMerchantDeals.map(findFirstDeal(_, changeSeq).getOrElse(0L)).sum
  }

  def dasBruttenForcen(secretNums: Vector[Long]): (Vector[Long], Long) = {
    val pricesWithChangesByMerchant = secretNums.map(calcPricesWithChanges).map(_.pricesWithChanges)
    val allPossibleChangeSeqs = pricesWithChangesByMerchant.flatten.map(_._2).toSet

    import scala.collection.parallel.CollectionConverters._

    allPossibleChangeSeqs.toVector.zipWithIndex.par.map { (changeSeq, ix) =>
      if (ix % 1000 == 0) println(ix)
      val totalPrice = calcTotalPrice(pricesWithChangesByMerchant, changeSeq)
      changeSeq -> totalPrice
    }.maxBy(_._2)
  }

  def task1(): Long = {
    val secretNums = readInput("day22.txt")

    secretNums.map { secretNum =>
      Vector.iterate(secretNum, 2000 + 1)(calcNext).last
    }.sum
  }

  def task2(): Long = {
    val secretNums = readInput("day22.txt")
    val (changeSeq, totalPrice) = dasBruttenForcen(secretNums)

    println(changeSeq)
    totalPrice
  }
}
