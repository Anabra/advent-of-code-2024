package adventofcode2024

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

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

  case class UncompressedFileDescriptor(
    fileId: Int,
    beginIx: Int,
    numFilledBlocks: Int,
    numEmptyBlocks: Int,
  )

  case class CompressedFileDescriptor(
    fileId: Int,
    beginIx: Int,
    length: Int,
  )

  def compactFs(fs: Vector[UncompressedFileDescriptor]): Vector[CompressedFileDescriptor] = {
    val compressedFs = mutable.Queue.empty[CompressedFileDescriptor]

    def loop(
      filePtrToFreeSpaceTarget: Int,
      numFreeBlocksUsed: Int,
      filePtrToCompressionTarget: Int,
      numAlreadyCompressedBlocks: Int,
    ): Vector[CompressedFileDescriptor] = {
      val freeSpaceTarget = fs(filePtrToFreeSpaceTarget)
      val compressionTarget = fs(filePtrToCompressionTarget)

      val numAvailableBlocks = freeSpaceTarget.numEmptyBlocks - numFreeBlocksUsed
      val numNeededBlocks = compressionTarget.numFilledBlocks - numAlreadyCompressedBlocks
      val beginIxCompressedPart = freeSpaceTarget.beginIx + numFreeBlocksUsed

      if (filePtrToFreeSpaceTarget < filePtrToCompressionTarget) {
        if (numFreeBlocksUsed == 0) {
          compressedFs.enqueue(
            CompressedFileDescriptor(
              fileId = freeSpaceTarget.fileId,
              beginIx = freeSpaceTarget.beginIx,
              length = freeSpaceTarget.numFilledBlocks,
            )
          )
        }

        if (numAvailableBlocks > numNeededBlocks) {
          val newCompressedFd = CompressedFileDescriptor(
            fileId = compressionTarget.fileId,
            beginIx = beginIxCompressedPart,
            length = numNeededBlocks,
          )
          compressedFs.enqueue(newCompressedFd)
          loop(
            filePtrToFreeSpaceTarget,
            numFreeBlocksUsed + numNeededBlocks,
            filePtrToCompressionTarget - 1,
            0,
          )
        } else if (numAvailableBlocks < numNeededBlocks) {
          val newCompressedFd = CompressedFileDescriptor(
            fileId = compressionTarget.fileId,
            beginIx = beginIxCompressedPart,
            length = numAvailableBlocks,
          )
          compressedFs.enqueue(newCompressedFd)
          loop(
            filePtrToFreeSpaceTarget + 1,
            0,
            filePtrToCompressionTarget,
            numAvailableBlocks,
          )
        } else if (numAvailableBlocks == numNeededBlocks) {
          val newCompressedFd = CompressedFileDescriptor(
            fileId = compressionTarget.fileId,
            beginIx = beginIxCompressedPart,
            length = numAvailableBlocks,
          )
          compressedFs.enqueue(newCompressedFd)
          loop(
            filePtrToFreeSpaceTarget + 1,
            0,
            filePtrToCompressionTarget - 1,
            0,
          )
        } else {
          throw new Exception("should never happen")
        }
      } else if (filePtrToFreeSpaceTarget == filePtrToCompressionTarget && numNeededBlocks > 0) {
        val newCompressedFd = CompressedFileDescriptor(
          fileId = compressionTarget.fileId,
          beginIx = beginIxCompressedPart,
          length = numNeededBlocks,
        )
        compressedFs.enqueue(newCompressedFd)
        compressedFs.toVector
      } else {
        compressedFs.toVector
      }
    }

    loop(0, 0, fs.size - 1, 0)
  }

  def checksum(fs: Vector[CompressedFileDescriptor]): Long = {
    fs.map(fd => (fd.beginIx to fd.beginIx + fd.length).map(_ * fd.fileId.toLong).sum).sum
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
