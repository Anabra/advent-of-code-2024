package adventofcode2024

import adventofcode2024.common.*

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object Day17 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  case class State(
    a: Long,
    b: Long,
    c: Long,
    instructionPtr: Int,
    out: Vector[Int],
  ) {
    def nextInstruction: State = this.copy(instructionPtr = instructionPtr + 2)
    def prettyOut = out.mkString(",")
  }

  type Program = Vector[Int]

  enum OpCode {
    case ADV, BXL, BST, JNZ, BXC, OUT, BDV, CDV
  }

  import OpCode.*

  def readInput(): (Program, State) = {
    val bufferedSource = io.Source.fromResource("day17.txt")
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val registerPattern = """Register [ABC]: (-?\d+)""".r
    val programPattern = """Program: (.+)""".r

    val (registerLines, programLines) = lines.span(_.nonEmpty)
    val (regA, regB, regC) = registerLines.map {
      case registerPattern(rawRegValue) => rawRegValue.toLong
    } match {
      case Vector(a, b, c) => (a, b, c)
    }
    val program = programLines.drop(1).head match {
      case programPattern(rawProgram) => rawProgram.split(",").map(_.toInt).toVector
    }

    program -> State(regA, regB, regC, 0, Vector())
  }

  def evalComboOperand(state: State, operand: Int): Long =
    operand match {
      case x if (0 to 3).contains(x) => x
      case 4 => state.a
      case 5 => state.b
      case 6 => state.c
      case 7 => throw new Exception("Reserved operand!")
    }

  def evalDivision(state: State, evaluatedComboOperandValue: Long): Long = {
    val numerator = state.a
    val denominator = 1L << evaluatedComboOperandValue
    numerator / denominator
  }

  def evalOperation(state: State, operator: OpCode, operand: Int): State = {
    // it's lazy so that we dont get an exception if we try to evaluate combo operand 7
    lazy val combo = evalComboOperand(state, operand)
    val lit = operand

    operator match {
      case ADV =>
        val result = evalDivision(state, combo)
        state.copy(a = result).nextInstruction
      case BXL =>
        val result = state.b ^ lit
        state.copy(b = result).nextInstruction
      case BST =>
        val result = combo % 8
        state.copy(b = result).nextInstruction
      case JNZ =>
        if (state.a == 0) {
          state.nextInstruction
        } else {
          state.copy(instructionPtr = lit)
        }
      case BXC =>
        val result = state.b ^ state.c
        state.copy(b = result).nextInstruction
      case OUT =>
        val result = (combo % 8).toInt
        state.copy(out = state.out :+ result).nextInstruction
      case BDV =>
        val result = evalDivision(state, combo)
        state.copy(b = result).nextInstruction
      case CDV =>
        val result = evalDivision(state, combo)
        state.copy(c = result).nextInstruction
    }
  }

  @tailrec
  def evalProgram(program: Program, state: State): State = {
//    println(state)
    if (state.instructionPtr + 1 >= program.size) {
      state
    } else {
      val rawOpCode = program(state.instructionPtr)
      val opcode = OpCode.fromOrdinal(rawOpCode)
      val operand = program(state.instructionPtr + 1)
      val newState = evalOperation(state, opcode, operand)
      evalProgram(program, newState)
    }
  }

  @tailrec
  def evalProgramWithTermination(program: Program, state: State): Option[State] = {
    if (program.zip(state.out).exists(_ != _)) {
      None
    } else if (state.instructionPtr + 1 >= program.size) {
      if (program == state.out) {
        Some(state)
      } else {
        None
      }
    } else {
      val rawOpCode = program(state.instructionPtr)
      val opcode = OpCode.fromOrdinal(rawOpCode)
      val operand = program(state.instructionPtr + 1)
      val newState = evalOperation(state, opcode, operand)
      evalProgramWithTermination(program, newState)
    }
  }

  def crackProgram(program: Program, begin: Long = 0): Option[State] = {
    val baseState = State(0, 0, 0, 0, Vector())

    Iterator
      .iterate(begin)(_ + 1)
      .map(initA => baseState.copy(a = initA))
      .find { initState =>
        if (initState.a % 1000000 == 0) println(s"processing: ${initState.a}")
        evalProgramWithTermination(program, initState).isDefined
      }
  }

  type Bit = Int
  type BitTriplet = Vector[Bit] // always 3 bits, lowest bit first
  case class Constraint(
    beginIx: Int,
    fixedBits: Vector[Bit]
  ) {
    def indexedBits = fixedBits.zipWithIndex.map { case (bit, ix) => (bit, ix + beginIx)}
  }

  def flipAll(bits: BitTriplet): BitTriplet = {
    bits.zip(Vector(1, 1, 1)).map((a, b) => a ^ b)
  }

  def flipLastTwo(bits: BitTriplet): BitTriplet = {
    bits.zip(Vector(1, 1, 0)).map((a, b) => a ^ b)
  }

  def flipSnd(bits: BitTriplet): BitTriplet = {
    bits.zip(Vector(0, 1, 0)).map((a, b) => a ^ b)
  }

  def calcBase10(bitTriplet: BitTriplet): Long = {
    bitTriplet.reverse.foldLeft(0L) { case (acc, bit) => acc * 2 + bit }
  }

  def calcBitsPad3(numBase10: Int): Vector[Bit] = {
    numBase10.toBinaryString.map(_.asDigit).reverse.padTo(3, 0).toVector
  }

  // momo's
  def calcDirectConstraint(
    curLowestIndex: Int,
    aBits: BitTriplet,
    expected: BitTriplet
  ): Constraint = {
    val whatBWillBeBeforeXorC = flipSnd(aBits)
    val whatBWillBeAfterXorC = flipLastTwo(expected)
    val whatCShouldBe = whatBWillBeBeforeXorC.zip(whatBWillBeAfterXorC).map { case (a, b) => a ^ b }
    assert(whatBWillBeBeforeXorC.size == 3)
    val bAsBase10 = calcBase10(whatBWillBeBeforeXorC).toInt
    Constraint(curLowestIndex + bAsBase10, whatCShouldBe)
  }

  def applyConstraint(unsetBits: PartiallyUnsetBitVector, constraint: Constraint): Option[PartiallyUnsetBitVector] = {
    val newBitsOpt = constraint.indexedBits.foldLeft(Option(unsetBits.bits)) {
      case (None, _) => None
      case (acc@Some(curABits), (fixedBit, ix)) =>
        if (ix < unsetBits.bits.size) {
          if (curABits(ix).forall(_ == fixedBit)) {
            Some(curABits.updated(ix, Some(fixedBit)))
          } else {
            None
          }
        } else {
          if (fixedBit == 0) {
            acc
          } else {
            None
          }
        }
    }
    newBitsOpt.map(newBits => unsetBits.copy(bits = newBits))
  }

  def generatePossibleTriplets(partiallySetTriplet: Vector[Option[Bit]]): Set[Vector[Bit]] = {
    partiallySetTriplet.foldLeft(Set(Vector.empty[Bit])) { case (prefixesSoFar, curBitOpt) =>
      curBitOpt match {
        case Some(b) => prefixesSoFar.map(_ :+ b)
        case None => prefixesSoFar.map(_ :+ 0) ++ prefixesSoFar.map(_ :+ 1)
      }
    }
  }

  case class PartiallyUnsetBitVector(
    lowestUnprocessedIx: Int,
    bits: Vector[Option[Int]],
  ) {
    // TODO: cats traverse
    def resolve: Option[Vector[Int]] = {
      if (bits.forall(_.isDefined)) {
        Some(bits.map(_.get))
      } else {
        None
      }
    }

    def fixBits(fromIx: Int, bits: Vector[Bit]): PartiallyUnsetBitVector = {
      bits.map(Option.apply).zipWithIndex.foldLeft(this) { case (state, (curBit, relativeIx)) =>
        val absIx = relativeIx + fromIx
        state.copy(
          lowestUnprocessedIx = state.lowestUnprocessedIx + 1,
          bits = state.bits.updated(absIx, curBit),
        )
      }
    }
  }

  def calcNextPossibleStates(
    expectedBits: Vector[Bit],
    partiallyUnsetBits: PartiallyUnsetBitVector,
  ): Set[PartiallyUnsetBitVector] = {
    val curIndex = partiallyUnsetBits.lowestUnprocessedIx
    val aBits = partiallyUnsetBits.bits
    val expectedBitTriplet = expectedBits.slice(curIndex, curIndex + 3)
    val lowestUnsetBitTriplet = aBits.slice(curIndex, curIndex + 3)
    val possibleTriplets = generatePossibleTriplets(lowestUnsetBitTriplet)
    val newStatesWithConstraints = possibleTriplets.map { triplet =>
      val tripletApplied = partiallyUnsetBits.fixBits(curIndex, triplet)
      val constraint = calcDirectConstraint(curIndex, triplet, expectedBitTriplet)
      tripletApplied -> constraint
    }
    newStatesWithConstraints.flatMap { case (st, constraint) => applyConstraint(st, constraint) }
  }

  def krakkDaNumbaaah(expectedOutput: Vector[Int]): Long = {
    val expectedOutputBits = expectedOutput.flatMap(calcBitsPad3)
    val initABits = Vector.fill(expectedOutputBits.size)(Option.empty[Int])

    val todo = mutable.Queue(PartiallyUnsetBitVector(0, initABits))

    @tailrec
    def loop(visited: Set[PartiallyUnsetBitVector], solutions: Set[Vector[Bit]]): Long = {
      if (todo.isEmpty) {
        solutions.map(calcBase10).min
      } else {
        val curState = todo.dequeue()
        if (visited.contains(curState)) {
          loop(visited, solutions)
        } else {
          if (curState.lowestUnprocessedIx >= curState.bits.size) {
            curState.resolve match {
              case Some(resolvedBits) => loop(visited + curState, solutions + resolvedBits)
              case None => loop(visited + curState, solutions)
            }
          } else {
            val nextPossibilities = calcNextPossibleStates(expectedOutputBits, curState)
            todo.enqueueAll(nextPossibilities)
            loop(visited + curState, solutions)
          }
        }
      }
    }

    loop(Set.empty, Set.empty)
  }

  def task1(): String = {
    val (program, state) = readInput()
    val finalState = evalProgram(program, state)

    println(program)
    println(state)

    finalState.prettyOut
  }

  def task2(): Long = {
    val (program, state) = readInput()

    println("BEGIN")
    krakkDaNumbaaah(program)
  }
}
