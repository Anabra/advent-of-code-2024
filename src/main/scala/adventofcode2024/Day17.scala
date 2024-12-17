package adventofcode2024

import adventofcode2024.common.*

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

object Day17 {
  def main(args: Array[String]): Unit = {
//    println(task1())
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
    val bufferedSource = io.Source.fromResource("day17_momo.txt")
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

  def task1(): String = {
    val (program, state) = readInput()
    val finalState = evalProgram(program, state)

    println(program)
    println(state)

    finalState.prettyOut
  }

  def task2(): String = {
    val (program, state) = readInput()

//    println(program)
//    println(state)

    val begin = 35186035000000L

    println("BEGIN")
    crackProgram(program, begin).match {
      case None => "NOT FOUND"
      case Some(state) => state.a.toString
    }
  }
}

// not 7,2,4,7,0,3,7,1,3
// 281474976710656L / 8
