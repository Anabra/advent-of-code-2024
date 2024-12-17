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
    a: Int,
    b: Int,
    c: Int,
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
      case registerPattern(rawRegValue) => rawRegValue.toInt
    } match {
      case Vector(a, b, c) => (a, b, c)
    }
    val program = programLines.drop(1).head match {
      case programPattern(rawProgram) => rawProgram.split(",").map(_.toInt).toVector
    }

    program -> State(regA, regB, regC, 0, Vector())
  }

  def evalComboOperand(state: State, operand: Int): Int =
    operand match {
      case x if (0 to 3).contains(x) => x
      case 4 => state.a
      case 5 => state.b
      case 6 => state.c
      case 7 => throw new Exception("Reserved operand!")
    }

  def evalDivision(state: State, evaluatedComboOperandValue: Int): Int = {
    val numerator = state.a
    val denominator = 1 << evaluatedComboOperandValue
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
        val result = combo % 8
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

  def task1(): String = {
    val (program, state) = readInput()
    val finalState = evalProgram(program, state)

    println(program)
    println(state)

    finalState.prettyOut
  }

  def task2(): Int = {
    val fsIndex = readInput()
    42
  }
}

// not 7,2,4,7,0,3,7,1,3
