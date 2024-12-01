package adventofcode2024

object Day1 {
  def main(args: Array[String]): Unit = {
    println(task1())
  }

  def readInputForDay1Task1(): (Vector[Int], Vector[Int]) = {
    // read resource file for day 1 input
    val bufferedSource = io.Source.fromResource("day1_task1.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines
      .map(_.split("""\s+"""))
      .map { case Array(fst, snd) => (fst.toInt, snd.toInt) }
      .unzip
  }

  def task1(): Int = {
    val (lhsLocs, rhsLocs) = readInputForDay1Task1()

    lhsLocs.sorted
      .zip(rhsLocs.sorted)
      .map { case (fst, snd) => (fst - snd).abs }
      .sum
  }
}
