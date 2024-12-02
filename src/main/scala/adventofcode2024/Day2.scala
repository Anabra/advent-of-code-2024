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

  // TODO: clean up this madness
  def task2(): Int = {
    val reports = readInput()

    reports.count { report =>
      val reportTail = report.drop(1)
      val diffs = report
        .zip(reportTail)
        .map{ case (fst, snd) => snd - fst }

      val isIncreasing = (d: Int) => 1 <= d && d <= 3
      val isDecreasing = (d: Int) => -3 <= d && d <= -1

      val numTotal = diffs.size
      val numIncreasingGood = diffs.count(isIncreasing)
      val numDecreasingGood = diffs.count(isDecreasing)

      def shiftError(error: Int, reportSlice: Seq[Int]): Seq[Int] = {
        reportSlice match {
          case Nil => Nil
          case fst :: rest => (fst + error) :: rest
        }
      }

      def checkFoobar2(report: List[Int], pred: Int => Boolean): Boolean = {
        val errorIxOpt = Option(diffs.indexWhere(!pred(_))).filter(_ != -1)

        errorIxOpt.fold(true) { errorIx =>
          // NOTE: error will certainly exists, because found its index beforehand
          val (potentiallyGoodPrefix, error :: potentiallyGoodSuffix) = diffs.toList.splitAt(errorIx)
          val newReport1 = potentiallyGoodPrefix ++ shiftError(error, potentiallyGoodSuffix)
          val newReport2 = shiftError(error, potentiallyGoodPrefix.reverse).reverse ++ potentiallyGoodSuffix
          newReport1.forall(pred) || newReport2.forall(pred)
        }
      }

      val dampenedIncreasingGood = (numTotal == numIncreasingGood)
        || checkFoobar2(diffs.toList, isIncreasing)

      val dampenedDecreasingGood = (numTotal == numDecreasingGood)
        || checkFoobar2(diffs.toList, isDecreasing)

      dampenedIncreasingGood || dampenedDecreasingGood
    }
  }
}
