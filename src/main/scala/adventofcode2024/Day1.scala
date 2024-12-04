package adventofcode2024

object Day1 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  def readInputForDay1Task1(): (Vector[Int], Vector[Int]) = {
    val bufferedSource = io.Source.fromResource("day1.txt")
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

  def task2(): Int = {
    val (lhsLocs, rhsLocs) = readInputForDay1Task1()
    val rhsLocOccurrences = rhsLocs.groupBy(identity).view.mapValues(_.size).toMap

    lhsLocs
      .map(loc => loc * rhsLocOccurrences.getOrElse(loc, 0))
      .sum
  }
}
