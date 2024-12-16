package adventofcode2024

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object Day16 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  enum MazeObject {
    case Wall, Empty, Start, End

    def pretty: String = this match {
      case MazeObject.Wall => "#"
      case MazeObject.Empty => "."
      case MazeObject.Start => "S"
      case MazeObject.End => "E"
    }
  }

  type Maze = Vector[Vector[MazeObject]]

  def readInput(): Maze = {
    val bufferedSource = io.Source.fromResource("day16_small.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines.map(_.toVector.map {
      case '#' => MazeObject.Wall
      case '.' => MazeObject.Empty
      case 'S' => MazeObject.Start
      case 'E' => MazeObject.End
    })
  }

  def prettyMaze(maze: Maze): String = {
    maze.map(_.map(_.pretty).mkString).mkString("\n")
  }

  def task1(): Int = {
    val maze = readInput()
    println(prettyMaze(maze))

    42
  }

  def task2(): Int = {
    val fsIndex = readInput()
    42
  }
}
