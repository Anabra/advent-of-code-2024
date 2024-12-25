package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.*

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

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

  def task2(): Int = {
    val program = readInput("day24.txt")
    val endState = evaluate(program)

    val xs = convertXsToDecimal(program)
    val ys = convertYsToDecimal(program)
    val zs = convertZsToDecimal(endState)
    
    println(s"xs: ${xs}")
    println(s"ys: ${ys}")
    println(s"actual zs: ${zs}")
    println(s"expected zs: ${xs + ys}")
   
   42 
  }
}
