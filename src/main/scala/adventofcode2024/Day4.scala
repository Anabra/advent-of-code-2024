package adventofcode2024

object Day4 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  def readInput(): Vector[Vector[Char]] = {
    val bufferedSource = io.Source.fromResource("day4.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines.map(_.toVector)
  }

  def findLetter(wordMatrix: Vector[Vector[Char]], letter: Char): Vector[(Int, Int)] = {
    val xPattern = s"""${letter}""".r

    wordMatrix.zipWithIndex.flatMap { case (line, ix) =>
      val xSpots = xPattern.findAllMatchIn(line.mkString).map(_.start)
      xSpots.map((ix, _))
    }
  }

  def getSurroundingCoords(x: Int, y: Int, len: Int, maxRowLength: Int, maxColLength: Int): Vector[Vector[(Int, Int)]] = {
    Vector(
      Vector.iterate((x,y), len) { case (x,y) => (x, y + 1) }, // horizontal forward
      Vector.iterate((x,y), len) { case (x,y) => (x, y - 1) }, // horizontal backward
      Vector.iterate((x,y), len) { case (x,y) => (x + 1, y) }, // vertical down
      Vector.iterate((x,y), len) { case (x,y) => (x - 1, y) }, // vertical up
      Vector.iterate((x,y), len) { case (x,y) => (x + 1, y + 1) }, // diagonal down-right
      Vector.iterate((x,y), len) { case (x,y) => (x - 1, y - 1) }, // diagonal up-left
      Vector.iterate((x,y), len) { case (x,y) => (x + 1, y - 1) }, // diagonal down-left
      Vector.iterate((x,y), len) { case (x,y) => (x - 1, y + 1) } // diagonal up-right
    ).filter(_.forall { case (x,y) => 0 <= x && x < maxRowLength && 0 <= y && y < maxColLength })
  }

  def getWordFromCoords(coords: Vector[(Int, Int)], wordMatrix: Vector[Vector[Char]]): String = {
    coords.map { case (x, y) => wordMatrix(x)(y) }.mkString
  }

  def task1(): Int = {
    val input = readInput()

    findLetter(input, 'X').map { case (x,y) =>
      val surroundingCoords = getSurroundingCoords(x, y, 4, input.head.length, input.length)
      val words = surroundingCoords.map(getWordFromCoords(_, input))
      words.count(_ == "XMAS")
    }.sum
  }

  def getSurroundingXCoords(x: Int, y: Int,  maxRowLength: Int, maxColLength: Int): Vector[Vector[(Int, Int)]] = {
    Vector(
      Vector((x-1, y-1), (x,y), (x+1, y+1)), // diagonal down-right
      Vector((x-1, y+1), (x,y), (x+1, y-1)), // diagonal down-left
    ).filter(_.forall { case (x,y) => 0 <= x && x < maxRowLength && 0 <= y && y < maxColLength })
  }
  def task2(): Int = {
    val input = readInput()

    findLetter(input, 'A').count { case (x,y) =>
      val surroundingCoords = getSurroundingXCoords(x, y, input.head.length, input.length)
      val words = surroundingCoords.map(getWordFromCoords(_, input))
      words.count(Set("MAS", "SAM").contains(_)) == 2
    }
  }
}
