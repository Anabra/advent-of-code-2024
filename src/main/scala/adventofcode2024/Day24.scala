package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.*
import guru.nidi.graphviz.attribute.Attributes.attr
import guru.nidi.graphviz.attribute.{Color, Font, Rank, SimpleLabel, Style}
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

  def calcComputeOrder(gates: Vector[Gate]): Vector[VarName] = {
    val deps = calcDependencyGraph(gates)
    toposort(deps)
  }

  def calcComputeOrderFromGateConfig(gateConfig: GateConfiguration): Vector[VarName] = {
    val gates = gateConfig.toVector.map { case (outVar, gate) => Gate(gate.op, gate.lhs, gate.rhs, outVar) }
    calcComputeOrder(gates)
  }

  def reorderOperations(gates: Vector[Gate]): Vector[Gate] = {
    val varsInComputeOrder = calcComputeOrder(gates)
    gates.sortBy(op => varsInComputeOrder.indexOf(op.out))
  }

  def evaluateOp(vars: Map[VarName, Bit], op: Operation, lhs: VarName, rhs: VarName, out: VarName): Map[VarName, Bit] = {
    val lhsVal = vars(lhs)
    val rhsVal = vars(rhs)

    val res = op match {
      case And => lhsVal & rhsVal
      case Xor => lhsVal ^ rhsVal
      case Or  => lhsVal | rhsVal
    }

    vars.updated(out, res)
  }

  def evaluateProgram(program: Program): Map[VarName, Bit] = {
    val reorderedOps = reorderOperations(program.gates)
    reorderedOps.foldLeft(program.inputs) { case (vars, gate) =>
      evaluateOp(vars, gate.op, gate.lhs, gate.rhs, gate.out)
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

    val endState = evaluateProgram(program)

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
    val endState = evaluateProgram(program)
    val zs = convertZsToDecimal(endState)
    zs == xs + ys
  }

  def findFailingInputs(gateConfig: GateConfiguration, computeOrder: Vector[VarName]): LazyList[(Long, Long, Long)] = {
    val inputs = generateNewInputs(10)

    LazyList.from(inputs).map { case (xs, ys) =>
      val endState = evaluateGateConfig(gateConfig, computeOrder, calcInputs(xs, ys))
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

  def genGraphivLinksFromGateConfig(gateConfig: GateConfiguration): Vector[Node] =
    gateConfig.toVector.flatMap { case (outVar, gate) =>
      val nodeLabel = s"${gate.op.toString.toUpperCase} -> ${outVar}"

      Vector(
        gviz.node(outVar).`with`("label", nodeLabel).link(gviz.node(gate.lhs)),
        gviz.node(outVar).`with`("label", nodeLabel).link(gviz.node(gate.rhs)),
      )
    }

  def visualizeWithGraphviz(gateConfiguration: GateConfiguration, suffix: String = ""): Unit = {
    val outvarToInvarsLinks = genGraphivLinksFromGateConfig(gateConfiguration)

    val outvarToInvars = gviz.graph("outvar-to-invars")
      .directed
      .graphAttr.`with`(Rank.dir(RIGHT_TO_LEFT))
      .nodeAttr.`with`(Font.name("arial"))
      .linkAttr.`with`("class", "link-class")
      .`with`(outvarToInvarsLinks*)

    val fileName = s"example/outvar-to-invars${suffix}.svg"
    Graphviz.fromGraph(outvarToInvars).height(100).render(Format.SVG).toFile(new File(fileName))
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

  def swapGatesInConfig(gateConfig: GateConfiguration, outVar1: VarName, outVar2: VarName): GateConfiguration = {
    val ogGateOutVar1 = gateConfig(outVar1)
    val ogGateOutVar2 = gateConfig(outVar2)

    gateConfig.updated(outVar1, ogGateOutVar2).updated(outVar2, ogGateOutVar1)
  }

  def swapGatesInComputeOrder(order: Vector[VarName], outVar1: VarName, outVar2: VarName): Vector[VarName] = {
    val outVar1Ix = order.indexOf(outVar1)
    val outVar2Ix = order.indexOf(outVar2)

    order
      .updated(outVar1Ix, outVar2)
      .updated(outVar2Ix, outVar1)
  }

  def evaluateGateConfig(
    config: GateConfiguration,
    computeOrder: Vector[VarName],
    inputs: Map[VarName, Bit]
  ): Map[VarName, Bit] = {
    computeOrder.foldLeft(inputs) { case (curState, curVarToCompute) =>
      inputs.get(curVarToCompute) match {
        case None =>
          val gate = config(curVarToCompute)
          evaluateOp(curState, gate.op, gate.lhs, gate.rhs, curVarToCompute)
        // it was an input variable
        case Some(inputValue) =>
          curState.updated(curVarToCompute, inputValue)
      }
    }
  }

  def swapManyGatesInConfig(gateConfig: GateConfiguration, swaps: Vector[(VarName, VarName)]): GateConfiguration =
    swaps.foldLeft(gateConfig) { case (gCfg, (outVar1, outVar2)) =>
      swapGatesInConfig(gCfg, outVar1, outVar2)
    }

  def swapManyGatesInComputeOrder(computeOrder: Vector[VarName], swaps: Vector[(VarName, VarName)]): Vector[VarName] =
    swaps.foldLeft(computeOrder) { case (curOrder, (outVar1, outVar2)) =>
      swapGatesInComputeOrder(curOrder, outVar1, outVar2)
    }

  def evaluateGateConfigWithSwaps(
    gateConfig: GateConfiguration,
    ogComputeOrder: Vector[VarName],
    inputs: Map[VarName, Bit],
    swaps: Vector[(VarName, VarName)],
  ): Map[VarName, Bit] = {
    val newGateConfig = swapManyGatesInConfig(gateConfig, swaps)
    val newComputeOrder = swapManyGatesInComputeOrder(ogComputeOrder, swaps)

    evaluateGateConfig(newGateConfig, newComputeOrder, inputs)
  }

  // map of outvar -> invars that depend on invar
  def calcInverseDependencyGraph(gates: Vector[Gate]): Map[VarName, Set[VarName]] =
    calcGateConfig(gates).mapVals { case GateWithoutOutVar(_, lhs, rhs) => Set(lhs, rhs) }

  def calcInverseTransitiveDependencyGraph(gates: Vector[Gate]): Map[VarName, Set[VarName]] =
    fix(calcInverseDependencyGraph(gates)) { depGraph =>
      depGraph.map { case (outVar, inVars) =>
        val inVarsOfInVars = inVars.flatMap(depGraph.getOrElse(_, Set()))
        outVar -> (inVars ++ inVarsOfInVars)
      }
    }

  def calcTransitiveDependencyGraph(gates: Vector[Gate]): Map[VarName, Set[VarName]] =
    fix(calcDependencyGraph(gates)) { depGraph =>
      depGraph.map { case (inVar, outVars) =>
        val outVarsOfOutVars = outVars.flatMap(depGraph.getOrElse(_, Set()))
        inVar -> (outVars ++ outVarsOfOutVars)
      }
    }

  def calcSwappabilityGraph(gates: Vector[Gate]): Map[VarName, Set[VarName]] = {
    val allOutVars = gates.map(_.out).toSet
    val inverseTransitiveDeps = calcInverseTransitiveDependencyGraph(gates)
    val transitiveDeps = calcTransitiveDependencyGraph(gates)
    allOutVars.map { outVar =>
      val transitiveInputs = inverseTransitiveDeps.getOrElse(outVar, Set())
      val transitiveOutputs = transitiveDeps.getOrElse(outVar, Set())
      outVar -> (allOutVars -- transitiveInputs -- transitiveOutputs)
    }.toMap
  }

  def task2(): Int = {
    val ogProgram = readInput("day24.txt")
    val ogGateConfig = calcGateConfig(ogProgram.gates)
    val ogComputeOrder = calcComputeOrder(ogProgram.gates)

    visualizeWithGraphviz(ogGateConfig)

    val swaps = Vector(
      "z18" -> "hmt",
      "z27" -> "bfq",
      "z31" -> "hkh",
      "fjp" -> "bng",
    )
    println(swaps.flatMap { case (lhs, rhs) => Vector(lhs, rhs) }.sorted.mkString(","))
    val swappedGateConfig = swapManyGatesInConfig(ogGateConfig, swaps)
    val swappedComputeOrder = calcComputeOrderFromGateConfig(swappedGateConfig)
    visualizeWithGraphviz(swappedGateConfig, "-swapped")

    findFailingInputs(swappedGateConfig, swappedComputeOrder).foreach { case (xs, ys, actualZs) =>
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
