package adventofcode2024

import scala.annotation.{nowarn, tailrec}

object Day9 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  case class FileDescriptor(
    beginningIx: Int,
    length: Int,
  )

  // file id -> file descriptor
  type FileSystemIndex = Vector[FileDescriptor]

  def readInput(): FileSystemIndex = {
    val bufferedSource = io.Source.fromResource("day9_small.txt")
    val input = bufferedSource.getLines.toVector.head.map(_.asDigit).toVector.grouped(2)
    bufferedSource.close

    input.foldLeft(0 -> Vector.empty[FileDescriptor]) { case ((curIx, descriptorAcc), foobar) =>
      foobar match {
        case Vector(numFilledBlocks, numEmptyBlocks) =>
          val newIx = curIx + numFilledBlocks + numEmptyBlocks
          val newDescriptor = FileDescriptor(curIx, numFilledBlocks)
          newIx -> (descriptorAcc :+ newDescriptor)
        case Vector(numFilledBlocks) =>
          val newIx = curIx + numFilledBlocks // this doesn't really matter
          val newDescriptor = FileDescriptor(curIx, numFilledBlocks)
          newIx -> (descriptorAcc :+ newDescriptor)
      }
    }._2
  }

  def prettyFileSystem(fsIndex: FileSystemIndex): String = {
    def prettyFileId(id: Int): String = {
      if (id < 10) id.toString else s"[${id}]"
    }

    val dummyFd = FileDescriptor(0, 0)
    fsIndex.zipAll(fsIndex.drop(1), dummyFd, dummyFd).zipWithIndex.map { case ((curFd, nextFd), fileId) =>
      val filledBlocks = Vector.fill(curFd.length)(prettyFileId(fileId)).mkString
      val emptyBlocks = Vector.fill(nextFd.beginningIx - (curFd.beginningIx + curFd.length))(".").mkString
      filledBlocks + emptyBlocks
    }.mkString
  }

  def task1(): Int = {
    val fsIndex = readInput()
    println(fsIndex)
    println(prettyFileSystem(fsIndex))

    42
  }

  def task2(): Int = {
    val fsIndex = readInput()
    42
  }
}
/*
00...111...2...333.44.5555.6666.777.888899
00...111...2...333.44.5555.6666.777.888899
*/
