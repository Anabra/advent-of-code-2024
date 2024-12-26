package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.*
import guru.nidi.graphviz.attribute.Attributes.attr
import guru.nidi.graphviz.attribute.{Color, Font, Rank, Style}
import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.{Node, Factory as gviz}
import guru.nidi.graphviz.attribute.Rank.RankDir.*

import java.io.File
import java.nio.channels.GatheringByteChannel
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

  enum Operation {
    case And, Xor, Or
  }
  import Operation.*

  case class Gate(
    op: Operation,
    lhs: VarName,
    rhs: VarName,
    out: VarName,
  )

  case class Program(
    inputs: Map[VarName, Bit],
    gates: Vector[Gate],
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
          case "AND" => Gate(And, lhsName, rhsName, outputName)
          case "XOR" => Gate(Xor, lhsName, rhsName, outputName)
          case "OR" => Gate(Or, lhsName, rhsName, outputName)
        }
    }

    Program(initialVars, gates)
  }

  // map of invar -> outvars that depend on invar
  def calcDependencyGraph(operations: Vector[Gate]): Map[VarName, Set[VarName]] =
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

  def reorderOperations(operations: Vector[Gate]): Vector[Gate] = {
    val deps = calcDependencyGraph(operations)
    val varsInComputeOrder = toposort(deps)
    operations.sortBy(op => varsInComputeOrder.indexOf(op.out))
  }

  def evaluate(program: Program): Map[VarName, Bit] = {
    val reorderedOps = reorderOperations(program.gates)
    reorderedOps.foldLeft(program.inputs) { case (vars, op) =>
      val res = op match {
        case Gate(And, lhs, rhs, _) => vars(lhs) & vars(rhs)
        case Gate(Xor, lhs, rhs, _) => vars(lhs) ^ vars(rhs)
        case Gate(Or, lhs, rhs, _)  => vars(lhs) | vars(rhs)
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
    val program = readInput("day24.txt")

    program.inputs.toVector.sorted.foreach { case (varName, value) =>
      println(s"${varName}: ${value}")
    }
    println()

    val reorderedGates = reorderOperations(program.gates)
    reorderedGates.foreach(println)

    val endState = evaluate(program)

    val res = calcFinalAnswer(endState)
    assert(res == 51745744348272L)
    res
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

  def findFailingInputs(gates: Vector[Gate]): LazyList[(Long, Long, Long)] = {
    val inputs = generateNewInputs(10)

    LazyList.from(inputs).map { case (xs, ys) =>
      val program = Program(inputs = calcInputs(xs, ys), gates = gates)
      val endState = evaluate(program)
      val zs = convertZsToDecimal(endState)
      (xs, ys, zs)
    }.filter { case (xs, ys, actualZs) => actualZs != xs + ys }
  }

  def findMismatchingBits(actual: Long, expected: Long): Vector[Int] = {
    actual.toBinaryString.reverse
      .zip(expected.toBinaryString.reverse)
      .zipWithIndex
      .collect { case ((actualBit, expectedBit), ix) if actualBit != expectedBit => ix }
      .toVector
  }

  def genGraphivLinks(dependencyGraph: Map[VarName, Set[VarName]]): Vector[Node] =
    dependencyGraph.toVector.flatMap { case (curNode, deps) =>
      deps.map(dep => gviz.node(curNode).link(gviz.node(dep)))
    }

  def visualizeWithGraphviz(program: Program): Unit = {
    val invarToOutvarsLinks = genGraphivLinks(calcDependencyGraph(program.gates))
    val outvarToInvarsLinks = genGraphivLinks(calcInverseDependencyGraph(program.gates))

    val invarToOutvars = gviz.graph("invar-to-outvars")
      .directed
      .graphAttr.`with`(Rank.dir(LEFT_TO_RIGHT))
      .nodeAttr.`with`(Font.name("arial"))
      .linkAttr.`with`("class", "link-class")
      .`with`(invarToOutvarsLinks*)

    val outvarToInvars = gviz.graph("outvar-to-invars")
      .directed
      .graphAttr.`with`(Rank.dir(RIGHT_TO_LEFT))
      .nodeAttr.`with`(Font.name("arial"))
      .linkAttr.`with`("class", "link-class")
      .`with`(outvarToInvarsLinks*)

    Graphviz.fromGraph(invarToOutvars).height(100).render(Format.SVG).toFile(new File("example/invar-to-outvars.svg"))
    Graphviz.fromGraph(outvarToInvars).height(100).render(Format.SVG).toFile(new File("example/outvar-to-invars.svg"))
  }

  case class GateWithoutOutVar(
    op: Operation,
    lhs: VarName,
    rhs: VarName,
  )

  // outvar -> op + invars
  type GateConfiguration = Map[VarName, GateWithoutOutVar]

  def calcGateConfig(gates: Vector[Gate]): GateConfiguration =
    gates.foldLeft(Map.empty) { case (gateConfig, curGate) =>
      gateConfig
        .updatedWith(curGate.out) {
          case None => Some(GateWithoutOutVar(curGate.op, curGate.lhs, curGate.rhs))
          case _ => throw new Exception("shouldnt happen")
        }
    }

  // map of outvar -> invars that depend on invar
  def calcInverseDependencyGraph(gates: Vector[Gate]): Map[VarName, Set[VarName]] =
    calcGateConfig(gates).mapVals { case GateWithoutOutVar(_, lhs, rhs) => Set(lhs, rhs) }

  def task2(): Int = {
    val ogProgram = readInput("day24.txt")
    visualizeWithGraphviz(ogProgram)

//    val newProgram = overrideInputs(ogProgram, xs = 12420713017224L, ys = 18578244294226L)
//    val endState = evaluate(newProgram)
//    println(convertZsToDecimal(endState))
//    println(12420713017224L + 18578244294226L)

    findFailingInputs(ogProgram.gates).foreach { case (xs, ys, actualZs) =>
      val expectedZs = xs + ys

      println(s"xs: ${xs}")
      println(s"ys: ${ys}")

      println(s"actual zs: ${actualZs}")
      println(s"expected zs: ${expectedZs}")

      println(s"${actualZs.toBinaryString}")
      println(s"${expectedZs.toBinaryString}")
      println(findMismatchingBits(actualZs, expectedZs))

      println()
    }


//    assert(decimalTo45Bits(xs) == program.inputs.toVector.filter((v,_) => v.startsWith("x")).sorted.map(_._2))
//    assert(decimalTo45Bits(ys) == program.inputs.toVector.filter((v,_) => v.startsWith("y")).sorted.map(_._2))

   42
  }
}
