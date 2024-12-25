package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.*

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import scala.util.Random

object Day24 {
  def main(args: Array[String]): Unit = {
//    println(task1())
    println(task2())
  }

  type VarName = String
  type Bit = Int

  sealed trait Operation {
    val lhs: VarName
    val rhs: VarName
    val out: VarName
  }
  case class And(lhs: VarName, rhs: VarName, out: VarName) extends Operation
  case class Or(lhs: VarName, rhs: VarName, out: VarName) extends Operation
  case class Xor(lhs: VarName, rhs: VarName, out: VarName) extends Operation

  case class Program(
    inputs: Map[VarName, Bit],
    gates: Vector[Operation],
  )

  def readInput(path: String): Program = {
    val bufferedSource = io.Source.fromResource(path)
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val varNamePat = """\w+""".r
    val varAssignmentPat = s"""(${varNamePat}): ([01])""".r
    val gatePat = s"""(${varNamePat}) (AND|XOR|OR) (${varNamePat}) -> (${varNamePat})""".r

    val (rawVars, _ +: rawGates) = lines.span(_.nonEmpty)

    val initialVars = rawVars.map {
      case varAssignmentPat(varName, rawValue) => varName -> rawValue.toInt
    }.toMap

    val gates = rawGates.map {
      case gatePat(lhsName, rawGate, rhsName, outputName) =>
        rawGate match {
          case "AND" => And(lhsName, rhsName, outputName)
          case "XOR" => Xor(lhsName, rhsName, outputName)
          case "OR" => Or(lhsName, rhsName, outputName)
        }
    }

    Program(initialVars, gates)
  }

  // map of invar -> outvars that depend on invar
  def calcDependencies(operations: Vector[Operation]): Map[VarName, Set[VarName]] =
    operations.foldLeft(Map.empty[VarName, Set[VarName]]) { case (deps, curOp) =>
      deps
        .updatedWith(curOp.lhs) {
          case None => Some(Set(curOp.out))
          case Some(outs) => Some(outs + curOp.out)
        }
        .updatedWith(curOp.rhs) {
          case None => Some(Set(curOp.out))
          case Some(outs) => Some(outs + curOp.out)
        }
    }

  def reorderOperations(operations: Vector[Operation]): Vector[Operation] = {
    val deps = calcDependencies(operations)
    val varsInComputeOrder = toposort(deps)
    operations.sortBy(op => varsInComputeOrder.indexOf(op.out))
  }

  def evaluate(program: Program): Map[VarName, Bit] = {
    val reorderedOps = reorderOperations(program.gates)
    reorderedOps.foldLeft(program.inputs) { case (vars, op) =>
      val res = op match {
        case And(lhs, rhs, _) => vars(lhs) & vars(rhs)
        case Xor(lhs, rhs, _) => vars(lhs) ^ vars(rhs)
        case Or(lhs, rhs, _)  => vars(lhs) | vars(rhs)
      }
      vars.updated(op.out, res)
    }
  }

  def bitsToDecimal(bitsReverse: Vector[Bit]): Long =
    bitsReverse.foldLeft(0L)((acc, cur) => acc * 2 + cur)

  def calcFinalAnswer(evalRes: Map[VarName, Bit]): Long = {
    val bits = evalRes.toVector.filter(_._1.startsWith("z")).sorted.reverse.map(_._2)
    bitsToDecimal(bits)
  }

  def task1(): Long = {
    val program = readInput("day24_small.txt")

    program.inputs.toVector.sorted.foreach { case (varName, value) =>
      println(s"${varName}: ${value}")
    }
    println()

    val reorderedGates = reorderOperations(program.gates)
    reorderedGates.foreach(println)

    val endState = evaluate(program)

    calcFinalAnswer(endState)
  }

  def convertVarsToDecimal(vars: Map[VarName, Bit], pred: VarName => Boolean): Long = {
    val bits = vars.toVector.filter((v,_) => pred(v)).sorted.reverse.map(_._2)
    bitsToDecimal(bits)
  }

  def convertXsToDecimal(program: Program): Long =
    convertVarsToDecimal(program.inputs, _.startsWith("x"))

  def convertYsToDecimal(program: Program): Long =
    convertVarsToDecimal(program.inputs, _.startsWith("y"))

  val convertZsToDecimal: Map[VarName, Bit] => Long = calcFinalAnswer

  def decimalTo45Bits(decimal: Long): Vector[Bit] =
    decimal.toBinaryString.reverse.padTo(45, '0').toVector.map(_.asDigit)

  def convertDecimalToBitVars(decimal: Long, varNamePrefix: String): Map[VarName, Bit] = decimalTo45Bits(decimal)
    .zipWithIndex
    .map { case (bitVal, ix) =>
      val paddedIx = ix.toString.reverse.padTo(2, '0').reverse
      val varName = varNamePrefix + paddedIx
      varName -> bitVal
    }
    .toMap

  def calcInputs(xs: Long, ys: Long): Map[VarName, Bit] = {
    val newXsBitVars = convertDecimalToBitVars(xs, "x")
    val newYsBitVars = convertDecimalToBitVars(ys, "y")
    newXsBitVars ++ newYsBitVars
  }

  def overrideInputs(program: Program, xs: Long, ys: Long): Program = {
    val newInputs = program.inputs ++ calcInputs(xs, ys)
    program.copy(inputs = newInputs)
  }

  def generateNewInputs(numInputs: Int): Vector[(Long, Long)] = {
    val rnd = new Random(42)
    val upperBoundExclusive = math.pow(2,45).toLong

    val maxValue = upperBoundExclusive - 1
    val minValue = 0L

    val totallyRandomInputs = (1 to numInputs).flatMap { _ =>
      val xs = rnd.nextLong(upperBoundExclusive)
      val ys = rnd.nextLong(upperBoundExclusive)
      Vector(
        xs -> ys,
        ys -> xs,
      )
    }

    val minMaxRandomInputs = (1 to 10).flatMap { _ =>
      val other = rnd.nextLong(upperBoundExclusive)

      Vector(
        other -> minValue,
        minValue -> other,
        other -> maxValue,
        maxValue -> other,
      )
    }

    val symmetricRandomInputs = (1 to 10).map { _ =>
      val other = rnd.nextLong(upperBoundExclusive)
      other -> other
    }

    val intervalEndInputs = Vector(
      minValue -> minValue,
      maxValue -> maxValue,
      minValue -> maxValue,
      maxValue -> minValue,
    )

    (totallyRandomInputs ++ minMaxRandomInputs ++ symmetricRandomInputs ++ intervalEndInputs).toVector
  }

  def isValidProgram(program: Program, xs: Long, ys: Long): Boolean = {
    val endState = evaluate(program)
    val zs = convertZsToDecimal(endState)
    zs == xs + ys
  }

  def findFailingInput(gates: Vector[Operation]): Option[(Long, Long, Long)] = {
    val inputs = generateNewInputs(10)

    inputs.map { case (xs, ys) =>
      val program = Program(inputs = calcInputs(xs, ys), gates = gates)
      val endState = evaluate(program)
      val zs = convertZsToDecimal(endState)
      (xs, ys, zs)
    }.find { case (xs, ys, actualZs) => actualZs != xs + ys }
  }

  def task2(): Int = {
    val ogProgram = readInput("day24.txt")

//    val newProgram = overrideInputs(ogProgram, xs = 12420713017224L, ys = 18578244294226L)
//    val endState = evaluate(newProgram)
//    println(convertZsToDecimal(endState))
//    println(12420713017224L + 18578244294226L)

    findFailingInput(ogProgram.gates).foreach { case (xs, ys, actualZs) =>
      println(s"xs: ${xs}")
      println(s"ys: ${ys}")
      println(s"actual zs: ${actualZs}")
      println(s"expected zs: ${xs + ys}")
    }


//    assert(decimalTo45Bits(xs) == program.inputs.toVector.filter((v,_) => v.startsWith("x")).sorted.map(_._2))
//    assert(decimalTo45Bits(ys) == program.inputs.toVector.filter((v,_) => v.startsWith("y")).sorted.map(_._2))

   42
  }
}
