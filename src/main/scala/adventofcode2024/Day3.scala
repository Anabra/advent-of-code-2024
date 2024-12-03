package adventofcode2024

object Day3 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  def readInput(): String = {
    val bufferedSource = io.Source.fromResource("day3.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines.mkString("")
  }

  def task1(): Int = {
    val input = readInput()

    val pattern = """mul\((\d+),(\d+)\)""".r
    pattern.findAllMatchIn(input).map { regexMatch =>
      regexMatch.subgroups.map(_.toInt).product
    }.sum
  }

  def task2(): Int = {
    val input = readInput()

    val mulPattern = """mul\((\d+),(\d+)\)""".r
    val doPattern = """(do\(\))""".r
    val dontPattern = """(don't\(\))""".r
    val combinedPattern = s"""${mulPattern.regex}|${doPattern.regex}|${dontPattern.regex}""".r

    combinedPattern.findAllMatchIn(input).foldLeft(true -> 0) { case ((shouldAdd, sum), regexMatch) =>
      regexMatch.subgroups.map(Option.apply) match {
        case List(Some(lhs), Some(rhs), None,    None) if shouldAdd => true -> (sum + lhs.toInt * rhs.toInt)
        case List(None,      None,      Some(_), None)              => true -> sum
        case List(None,      None,      None,    Some(_))           => false -> sum
        case _                                                      => shouldAdd -> sum
      }
    }._2
  }
}
