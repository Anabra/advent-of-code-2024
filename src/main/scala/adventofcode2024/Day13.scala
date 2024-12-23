package adventofcode2024

import java.util.regex.Pattern
import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import scala.util.matching.Regex

object Day13 {
  def main(args: Array[String]): Unit = {
    assert(task1() == 25751L)
    assert(task2() == 108528956728655L)
    testEdgeCases()
    println("All tests passed!")
  }

  case class Input(
    a: (Long, Long),
    b: (Long, Long),
    t: (Long, Long),
    aCost: Long,
    bCost: Long,
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
        3,
        1,
      )
    }.toVector
  }

  case class LinearlyDependentInput(
    smaller: (Long, Long),
    multiplier: Long,
    smallerCost: Long,
    biggerCost: Long,
    target: (Long, Long),
  )

  def calcLinearDependency(input: Input): Option[LinearlyDependentInput] = {
    val ((smaller, smallerCost), (bigger, biggerCost)) = if (input.a._1 < input.b._1){
      (input.a -> input.aCost, input.b -> input.bCost)
    }
    else {
      (input.b -> input.bCost, input.a -> input.aCost)
    }
    val (smaller1, smaller2) = smaller
    val (bigger1, bigger2) = bigger

    if (bigger1 % smaller1 == 0 && bigger2 % smaller2 == 0) {
      val multiplier = bigger1 / smaller1
      Some(
        LinearlyDependentInput(
          smaller = smaller,
          multiplier = multiplier,
          smallerCost = smallerCost,
          biggerCost = biggerCost,
          target = input.t,
        )
      )
    } else {
      None
    }
  }

  def solveLinearlyDependentInput(linDepInput: LinearlyDependentInput, originalInput: Input): Option[(Long, Long)] = {
    val LinearlyDependentInput(
      (smaller1, smaller2),
      multiplier,
      smallerCost,
      biggerCost,
      (t1, t2)
    ) = linDepInput

    if (t1 % smaller1 == 0 && t2 % smaller2 == 0) {
      val (bigger1, bigger2) = (smaller1 * multiplier, smaller2 * multiplier)
      val smallerIsProportionallyCheaper = multiplier * smallerCost < biggerCost
      val biggerDivides = t1 % bigger1 == 0 && t2 % bigger2 == 0

      val (smallerCount, biggerCount) = if (smallerIsProportionallyCheaper) {
        (t1 / smaller1) -> 0L
      } else {
        val biggerCount = t1 / bigger1
        val smallerCount = (t1 % bigger1) / smaller1
        smallerCount -> biggerCount
      }

      // swap it back
      if (originalInput.a == linDepInput.smaller) {
        Some(smallerCount -> biggerCount)
      } else {
        Some(biggerCount -> smallerCount)
      }
    } else {
      None
    }
  }

  def solveLinearlyIndependentInput(input: Input): Option[(Long, Long)] = {
    val (a1, a2) = input.a
    val (b1, b2) = input.b
    val (t1, t2) = input.t

    val n = (t1 * b2 - t2 * b1) / (a1 * b2 - a2 * b1)
    // TODO: handle b1 == 0
    val m = (t1 - n * a1) / b1

    val t1Recalculated = n * a1 + m * b1
    val t2Recalculated = n * a2 + m * b2

    if (t1Recalculated == t1 && t2Recalculated == t2) {
      Some((n, m))
    } else {
      None
    }
  }

  def calcSolution(input: Input): Option[(Long, Long)] = {
    calcLinearDependency(input) match {
      case Some(linDepInput) =>
        solveLinearlyDependentInput(linDepInput, input)
      case None =>
        solveLinearlyIndependentInput(input)
    }
  }

  def calcTokens(input: Input, aCount: Long, bCount: Long): Long =
    aCount * input.aCost + bCount * input.bCost

  def calcTotalPrice(inputs: Vector[Input]): Long =
    inputs
      .flatMap(in => calcSolution(in).map(in -> _))
      .map { case (input, (aCount, bCount)) => calcTokens(input, aCount, bCount) }
      .sum


  def task1(): Long = {
    val inputs = readInput()
    calcTotalPrice(inputs)
  }

  def task2(): Long = {
    val inputs = readInput()
    val extra = 10000000000000L
    val biiiiiiigInputs = inputs.map { case input =>
      val (t1, t2) = input.t
      input.copy(t = (t1 + extra, t2 + extra))
    }
    calcTotalPrice(biiiiiiigInputs)
  }

  def testEdgeCase(input: Input, expected: Option[(Long | Int, Long | Int)]): Unit = {
    val actual = calcSolution(input)
    assert(actual == expected, s"expected: $expected, actual: $actual")
  }

  def testEdgeCases(): Unit = {
    Seq(
      // smaller cheaper
      Input(
        a = (1, 1),
        b = (10, 10),
        t = (20, 20),
        aCost = 2,
        bCost = 21,
      ) -> Some(20 -> 0),

      // equal price
      Input(
        a = (1, 1),
        b = (10, 10),
        t = (20, 20),
        aCost = 2,
        bCost = 20,
      ) -> Some(0 -> 2),

      // bigger cheaper
      Input(
        a = (1, 1),
        b = (10, 10),
        t = (20, 20),
        aCost = 2,
        bCost = 19,
      ) -> Some(0 -> 2),

      // bigger cheaper but wont fit
      Input(
        a = (1, 1),
        b = (10, 10),
        t = (21, 21),
        aCost = 3,
        bCost = 2,
      ) -> Some(1 -> 2),

      // bigger cheaper, but only proportionally and it wont fit
      Input(
        a = (1, 1),
        b = (10, 10),
        t = (21, 21),
        aCost = 2,
        bCost = 10,
      ) -> Some(1 -> 2),

      // bigger cheaper, but cant be used at all
      Input(
        a = (1, 1),
        b = (40, 40),
        t = (20, 20),
        aCost = 3,
        bCost = 2,
      ) -> Some(20 -> 0),

      // can swap coords part 1
      Input(
        a = (1, 1),
        b = (10, 10),
        t = (20, 20),
        aCost = 3,
        bCost = 2,
      ) -> Some(0 -> 2),

      // can swap coords part 2
      Input(
        a = (10, 10),
        b = (1, 1),
        t = (20, 20),
        aCost = 3,
        bCost = 2,
      ) -> Some(2 -> 0),

    ).foreach { case (input, expected) =>
      testEdgeCase(input, expected)
    }
  }
}
