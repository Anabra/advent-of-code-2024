package adventofcode2024

import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable.SortedSet
import scala.collection.mutable

object Day9 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println("-------------------------")
    println(task2())
  }

  type FileId = Int

  case class UncompressedFileDescriptor(
    fileId: FileId,
    beginIx: Int,
    numFilledBlocks: Int,
    numEmptyBlocks: Int,
  )

  case class CompressedFileDescriptor(
    fileId: FileId,
    beginIx: Int,
    length: Int,
  )

  def readInput(): Vector[UncompressedFileDescriptor] = {
    val bufferedSource = io.Source.fromResource("day9.txt")
    val input = bufferedSource.getLines.toVector.head.map(_.asDigit).toVector.grouped(2)
    bufferedSource.close

    input.foldLeft((0, 0, Vector.empty[UncompressedFileDescriptor])) { case ((curFileId, curBeginIx, descriptorAcc), fileDesc) =>
      fileDesc match {
        case Vector(numFilledBlocks, numEmptyBlocks) =>
          val newIx = curBeginIx + numFilledBlocks + numEmptyBlocks
          val newDescriptor = UncompressedFileDescriptor(curFileId, curBeginIx, numFilledBlocks, numEmptyBlocks)
          (curFileId + 1, newIx, descriptorAcc :+ newDescriptor)
        case Vector(numFilledBlocks) =>
          val newIx = curBeginIx + numFilledBlocks // this doesn't really matter
          val newDescriptor = UncompressedFileDescriptor(curFileId, curBeginIx, numFilledBlocks, 0)
          (curFileId + 1, newIx, descriptorAcc :+ newDescriptor)
      }
    }._3
  }

  def prettyFileId(id: Int): String = {
    if (id < 10) id.toString else s"[${id}]"
  }

  def prettyUncompressedFileSystem(fsIndex: Vector[UncompressedFileDescriptor]): String =
    fsIndex.map { case UncompressedFileDescriptor(fileId, _, numFilledBlocks, numEmptyBlocks) =>
      val filledBlocks = Vector.fill(numFilledBlocks)(prettyFileId(fileId)).mkString
      val emptyBlocks = Vector.fill(numEmptyBlocks)(".").mkString
      filledBlocks + emptyBlocks
    }.mkString

  def prettyCompressedFileSystem(fsIndex: Vector[CompressedFileDescriptor]): String =
    fsIndex.map { case CompressedFileDescriptor(fileId, _, length) =>
      val filledBlocks = Vector.fill(length)(prettyFileId(fileId)).mkString
      filledBlocks
    }.mkString


  def compressFs(fs: Vector[UncompressedFileDescriptor]): Vector[CompressedFileDescriptor] = {
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
      val beginIxCompressedPart = freeSpaceTarget.beginIx + freeSpaceTarget.numFilledBlocks + numFreeBlocksUsed

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
            numAlreadyCompressedBlocks + numAvailableBlocks,
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
          beginIx = compressionTarget.beginIx,
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

  def calcChecksum(fs: Vector[CompressedFileDescriptor]): Long = {
    fs.map(fd => (fd.beginIx until fd.beginIx + fd.length).map(_ * fd.fileId.toLong).sum).sum
  }

  def task1(): Long = {
    val uncompressedFs = readInput()
    println(uncompressedFs)
    println(prettyUncompressedFileSystem(uncompressedFs))

    val compressedFs = compressFs(uncompressedFs)
    println(compressedFs)
    println(prettyCompressedFileSystem(compressedFs))

    calcChecksum(compressedFs)
  }

  case class ContiguousFreeSpace(
    beginIx: Int,
    length: Int,
  )

  type FreeSpaces = Vector[ContiguousFreeSpace]
  type UnfragmentedFileSystem = Map[FileId, CompressedFileDescriptor]

  def calcFreeSpaces(uncompressedFs: Vector[UncompressedFileDescriptor]): FreeSpaces =
    uncompressedFs.map(fd => ContiguousFreeSpace(fd.beginIx + fd.numFilledBlocks, fd.numEmptyBlocks))

  def calcUnfragmentedFileSystem(uncompressedFs: Vector[UncompressedFileDescriptor]): UnfragmentedFileSystem =
    uncompressedFs.map(fd => fd.fileId -> CompressedFileDescriptor(fd.fileId, fd.beginIx, fd.numFilledBlocks)).toMap

  def moveToFreeSpace(
    unusedfreeSpaces: FreeSpaces,
    fs: UnfragmentedFileSystem,
    fd: CompressedFileDescriptor,
  ): (FreeSpaces, UnfragmentedFileSystem) = {
    val neededSize = fd.length

    val (tooSmallFreeSpaces, rest) = unusedfreeSpaces.span(_.length < neededSize)

    rest match {
      case Vector(bigEnoughFreeSpace, rest*) if bigEnoughFreeSpace.beginIx < fd.beginIx =>
        val newFreeSpaces = if (bigEnoughFreeSpace.length == neededSize) {
          tooSmallFreeSpaces ++ rest
        } else {
          val modifiedBlock = ContiguousFreeSpace(
            beginIx = bigEnoughFreeSpace.beginIx + neededSize,
            length = bigEnoughFreeSpace.length - neededSize,
          )
          tooSmallFreeSpaces ++ Vector(modifiedBlock) ++ rest // NOTE: we don't put back the space we moved from
        }

        val newFs = fs.updated(fd.fileId, fd.copy(beginIx = bigEnoughFreeSpace.beginIx))

        (newFreeSpaces, newFs)
      case _ => (unusedfreeSpaces, fs)
    }
  }

  def compressWithoutFragmentation(
    freeSpaces: FreeSpaces,
    fs: UnfragmentedFileSystem,
  ): UnfragmentedFileSystem = {
    val sortedFileDescriptors = fs.values.toVector.sortBy(- _.fileId)

    sortedFileDescriptors.foldLeft(freeSpaces -> fs) { case ((unusedFreeSpaces, fs), curFd) =>
      moveToFreeSpace(unusedFreeSpaces, fs, curFd)
    }._2
  }

  def prettyUnfragmentedFs(fs: UnfragmentedFileSystem): String = {
    val fds = fs.values.toVector.sortBy(_.beginIx)
    val dummyFd = CompressedFileDescriptor(-1, 0, 0)
    fds.zipAll(fds.drop(1), dummyFd, dummyFd).map { case (curFd, nextFd) =>
      val curFdStr = prettyFileId(curFd.fileId) * curFd.length
      val freeSpaceStr = "." * (nextFd.beginIx - (curFd.beginIx + curFd.length))
      curFdStr + freeSpaceStr
    }.mkString
  }

  def task2(): Long = {
    val uncompressedFs = readInput()

    val freeSpaces = calcFreeSpaces(uncompressedFs)
    val unfragmentedFs = calcUnfragmentedFileSystem(uncompressedFs)

    val compressedUnfragmentedFs = compressWithoutFragmentation(freeSpaces, unfragmentedFs)

    println(prettyUnfragmentedFs(compressedUnfragmentedFs))

    calcChecksum(compressedUnfragmentedFs.values.toVector)
  }
}
/*
00...111...2...333.44.5555.6666.777.888899
00...111...2...333.44.5555.6666.777.888899

0099811188827773336446555566
0099811188827773336446555566

00992111777.44.333....5555.6666.....8888..
00992111777.44.333....5555.6666.....8888
*/
