package adventofcode2024

object Day2 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  def readInput(): Vector[Vector[Int]] = {
    val bufferedSource = io.Source.fromResource("day2.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines.map(_.split("""\s+""").toVector.map(_.toInt))
  }

  def task1(): Int = {
    val reports = readInput()

    reports.count { report =>
      val reportTail = report.drop(1)
      val diffs = report
        .zip(reportTail)
        .map(_ - _)

      diffs.forall(d => 1 <= d && d <= 3) || diffs.forall(d => -3 <= d && d <= -1)
    }
  }

  def parseTestInput(input: String): Vector[Int] = {
    input.trim.split("""\s+""").toVector.map(_.toInt)
  }

  def task2(): Int = {
    val reports1 = readInput()
    val reports2 = Vector(
      parseTestInput(
        """
          |12 14 15 18 20 21 22 23
          |""".stripMargin
      )
    )
    val reports3 = Vector(
      Vector(9, 9, 6, 5, 1)
    )
    val reports = reports3

    reports.count { report =>
      val reportTail = report.drop(1)
      val diffs = report
        .zip(reportTail)
        .map{ case (fst, snd) => snd - fst }

      val numTotal = diffs.size
      val numIncreasingGood = diffs.count(d => 1 <= d && d <= 3)
      val numDecreasingGood = diffs.count(d => -3 <= d && d <= -1)

      {
        val numGood = math.max(numIncreasingGood, numDecreasingGood)
        if (numGood == numTotal -2 || numGood == numTotal - 1) {
          println(s"${report} -> ${diffs}")
        }
      }

      val dampenedIncreasingGood = (numTotal == numIncreasingGood) || {
        val (goodPrefix, error :: potentiallyGoodSuffix) = diffs.toList.span(d => 1 <= d && d <= 3)
        val updatedSuffix = potentiallyGoodSuffix match {
          case Nil => Nil
          case fst :: rest => fst + error :: rest
        }
        val newReport = goodPrefix ++ updatedSuffix
        newReport.forall(d => 1 <= d && d <= 3)
      } || diffs.drop(1).forall(d => 1 <= d && d <= 3)

      val dampenedDecreasingGood = (numTotal == numDecreasingGood) || {
        val (goodPrefix, error :: potentiallyGoodSuffix) = diffs.toList.span(d => -3 <= d && d <= -1)
        val updatedSuffix = potentiallyGoodSuffix match {
          case Nil => Nil
          case fst :: rest => fst + error :: rest
        }
        val newReport = goodPrefix ++ updatedSuffix
        newReport.forall(d => -3 <= d && d <= -1)
      } || diffs.drop(1).forall(d => -3 <= d && d <= -1)

      dampenedIncreasingGood || dampenedDecreasingGood
    }
  }
}

/*
1 3 2 4 5
  2 -1 2 1

10 9 3 2 1
  -1 -6 -1 -1

10 3 2 1
  -7 -1 -1

5 10 9 8
  5 -1 -1

1 2 3 10
  1 1 7

10 5 4 3
  -5 -1 -1

5 10 6 5
  5 -4 -1

8 6 4 4 1
  -2 -2 0 -3

1 2 3 10 4 5
  1 1 7 -6 1
*/
