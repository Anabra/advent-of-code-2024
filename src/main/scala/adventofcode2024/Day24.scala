package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.*

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object Day24 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  type VarName = String
  type Bit = Int

  sealed trait Operation
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

  def task1(): Int = {
    val program = readInput("day24_small.txt")

    program.inputs.toVector.sorted.foreach { case (varName, value) =>
      println(s"${varName}: ${value}")
    }
    program.gates.foreach(println)


    42
  }

  def task2(): Int = {
    val program = readInput("day24_small.txt")
    42
  }
}
