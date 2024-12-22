package adventofcode2024

import adventofcode2024.Day21.NumberpadKey
import adventofcode2024.Day21.NumberpadKey.{Activate, Num}
import adventofcode2024.common.*
import adventofcode2024.common.graphs.*
import adventofcode2024.common.graphs.Dijkstra.*

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import scala.util.{Left, Right}

object Day21 {
  def main(args: Array[String]): Unit = {
    runTests()
//    println(task1())
//    println(task2())
  }

  enum NumberpadKey {
    case Num(n: Int)
    case Activate
    case Empty
  }

  enum ArrowpadKey {
    case Up, Down, Left, Right
    case Activate
    case Empty

    def pretty: String = this match {
      case Up => "^"
      case Down => "v"
      case Left => "<"
      case Right => ">"
      case Activate => "A"
      case Empty => "."
    }

    def toDirection: Coords = this match {
      case Up    => Coords(-1, 0)
      case Down  => Coords(1, 0)
      case Left  => Coords(0, -1)
      case Right => Coords(0, 1)
    }
  }

  case class State(
    numberpadPosition: Coords,
    arrowpadPositions: Vector[Coords], // nth is the nth arrowpad counted from the controller
    numberpadButtonsPressed: Vector[NumberpadKey],
  )

  type Codes = Vector[Vector[NumberpadKey]]

  val numberpad: Vector[Vector[NumberpadKey]] = Vector(
    Vector(NumberpadKey.Num(7), NumberpadKey.Num(8), NumberpadKey.Num(9)),
    Vector(NumberpadKey.Num(4), NumberpadKey.Num(5), NumberpadKey.Num(6)),
    Vector(NumberpadKey.Num(1), NumberpadKey.Num(2), NumberpadKey.Num(3)),
    Vector(NumberpadKey.Empty,  NumberpadKey.Num(0), NumberpadKey.Activate),
  )

  val numberpadEmptyPos = Coords(3,0)
  val numberpadActivatePos = Coords(3,2)

  val arrowpad: Vector[Vector[ArrowpadKey]] = Vector(
    Vector(ArrowpadKey.Empty, ArrowpadKey.Up,   ArrowpadKey.Activate),
    Vector(ArrowpadKey.Left,  ArrowpadKey.Down, ArrowpadKey.Right),
  )

  val arrowpadEmptyPos = Coords(0,0)
  val arrowpadActivatePos = Coords(0,2)

  def calcNextStateFromPressingActivate(st: State): Option[State] = {
    val (arrowpadsWithActivate, arrowpadsWithNotActivate) = st.arrowpadPositions.span(pos => arrowpad.atUnsafe(pos) == ArrowpadKey.Activate)
    arrowpadsWithNotActivate match {
      case Vector() =>
        val buttonsPressedSoFar = st.numberpadButtonsPressed
        numberpad.at(st.numberpadPosition).map { curButtonBeingPressed =>
          st.copy(numberpadButtonsPressed = buttonsPressedSoFar :+ curButtonBeingPressed)
        }
      case Vector(lastArrowpadPos) =>
        for {
          curArrowBeingPressed <- arrowpad.at(lastArrowpadPos)
          newNumberpadPos = st.numberpadPosition + curArrowBeingPressed.toDirection
          if newNumberpadPos != numberpadEmptyPos && newNumberpadPos.isWithinBounds(numberpad)
        } yield st.copy(numberpadPosition = newNumberpadPos)
      case Vector(fstNotActivateArrowpadPos, nextArrowpadCurPos, rest*) =>
        for {
          curArrowBeingPressed <- arrowpad.at(fstNotActivateArrowpadPos)
          nextArrowpadNewPos = nextArrowpadCurPos + curArrowBeingPressed.toDirection
          newArrowpadPositions = arrowpadsWithActivate ++ Vector(fstNotActivateArrowpadPos, nextArrowpadNewPos) ++ rest
          if nextArrowpadNewPos != arrowpadEmptyPos && nextArrowpadNewPos.isWithinBounds(arrowpad)
        } yield st.copy(arrowpadPositions = newArrowpadPositions)
    }
  }

  def calcNextMoves(code: Vector[NumberpadKey], prevMove: Move[State]): Set[Move[State]] = {
    val firstRemoteArrowpadPos +: otherRemoteArrowPadPoss = prevMove.to.arrowpadPositions
    val newStatesFromPressingArrowsOnDirectPad = {
      val adjacentKeyPositions = arrowpad.neighboursOf(firstRemoteArrowpadPos) - arrowpadEmptyPos
      adjacentKeyPositions.map { newfirstRemoteArrowpadPos =>
        prevMove.to.copy(arrowpadPositions = newfirstRemoteArrowpadPos +: otherRemoteArrowPadPoss)
      }
    }
    val newStateFromPressingActivate = calcNextStateFromPressingActivate(prevMove.to).toSet

    (newStatesFromPressingArrowsOnDirectPad ++ newStateFromPressingActivate)
      .filter(st => code.startsWith(st.numberpadButtonsPressed))
      .map { newState => Move(from = prevMove.to, to = newState, cost = prevMove.cost + 1) }
  }

  def readInput(path: String): Codes = {
    val bufferedSource = io.Source.fromResource(path)
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    lines.map { line =>
      line.toVector.map {
        case 'A' => NumberpadKey.Activate
        case key => NumberpadKey.Num(key.asDigit)
      }
    }
  }

  def genStartState(numRemoteArrowpads: Int): State = State(
    numberpadPosition = numberpadActivatePos,
    arrowpadPositions = Vector.fill(numRemoteArrowpads)(arrowpadActivatePos),
    numberpadButtonsPressed = Vector()
  )

  def calcArrowPressesForCode(numRemoteArrowPads: Int, code: Vector[NumberpadKey]): Vector[State] = {
    val exploration = Dijkstra.exploreSingleOptimalRoute[Vector[NumberpadKey], State](
      graph = code,
      start = genStartState(numRemoteArrowPads),
      isEnd = (st: State) => st.numberpadButtonsPressed == code,
      nextMoves = calcNextMoves
    ).get

    exploration.traceBackSingleOptimalPath.get
  }

  def calcComplexity(numRemoteArrowPads: Int, code: Vector[NumberpadKey]): Long = {
    val multiplier = code.dropRight(1).map { case Num(n) => n }.foldLeft(0) { case (acc, cur) => acc * 10 + cur }
    val numPressesNeeded = calcArrowPressesForCode(numRemoteArrowPads, code).size
    numPressesNeeded * multiplier
  }

  def requiredArrowKeysForNumberpad(startKey: NumberpadKey, endKey: NumberpadKey): Vector[ArrowpadKey] = {
    val startPos = numberpad.findElem(_ == startKey).get
    val endPos = numberpad.findElem(_ == endKey).get
    val diff = endPos - startPos

    val verticalDir = if (diff.x < 0) ArrowpadKey.Up else ArrowpadKey.Down
    val horizontalDir = if (diff.y < 0) ArrowpadKey.Left else ArrowpadKey.Right

    (verticalDir, horizontalDir) match {
      case (ArrowpadKey.Up, _) => Vector.fill(diff.x.abs)(ArrowpadKey.Up) ++ Vector.fill(diff.y.abs)(horizontalDir)
      case (_, ArrowpadKey.Right) => Vector.fill(diff.y.abs)(ArrowpadKey.Right) ++ Vector.fill(diff.x.abs)(verticalDir)
      case _ => Vector.fill(diff.x.abs)(verticalDir) ++ Vector.fill(diff.y.abs)(horizontalDir)
    }
  }

  def task1(): Long = {
    val codes = readInput("day21.txt")
    codes.map(code => calcComplexity(2, code)).sum
  }

  def task2(): Long = {
    val codes = readInput("day21_test.txt")
    codes.map(code => calcComplexity(25, code)).sum
  }

  def testArrowKeyGenFromNumpad(): Unit = {
    import ArrowpadKey as AK

    assert(requiredArrowKeysForNumberpad(NumberpadKey.Activate, Activate) == Vector())

    assert(requiredArrowKeysForNumberpad(NumberpadKey.Activate, NumberpadKey.Num(7)) == Vector(AK.Up, AK.Up, AK.Up, AK.Left, AK.Left))
    assert(requiredArrowKeysForNumberpad(NumberpadKey.Activate, NumberpadKey.Num(9)) == Vector(AK.Up, AK.Up, AK.Up))
    assert(requiredArrowKeysForNumberpad(NumberpadKey.Activate, NumberpadKey.Num(0)) == Vector(AK.Left))

    assert(requiredArrowKeysForNumberpad(NumberpadKey.Num(7), NumberpadKey.Activate) == Vector(AK.Right, AK.Right, AK.Down, AK.Down, AK.Down))
    assert(requiredArrowKeysForNumberpad(NumberpadKey.Num(7), NumberpadKey.Num(9)) == Vector(AK.Right, AK.Right))
    assert(requiredArrowKeysForNumberpad(NumberpadKey.Num(7), NumberpadKey.Num(1)) == Vector(AK.Down, AK.Down))

    assert(requiredArrowKeysForNumberpad(NumberpadKey.Num(5), NumberpadKey.Num(9)) == Vector(AK.Up, AK.Right))
    assert(requiredArrowKeysForNumberpad(NumberpadKey.Num(5), NumberpadKey.Num(7)) == Vector(AK.Up, AK.Left))
    assert(requiredArrowKeysForNumberpad(NumberpadKey.Num(5), NumberpadKey.Num(1)) == Vector(AK.Down, AK.Left))
    assert(requiredArrowKeysForNumberpad(NumberpadKey.Num(5), NumberpadKey.Num(3)) == Vector(AK.Right, AK.Down))
  }

  def runTests(): Unit = {
    testArrowKeyGenFromNumpad()

    println("TESTS PASSED")
  }
}
