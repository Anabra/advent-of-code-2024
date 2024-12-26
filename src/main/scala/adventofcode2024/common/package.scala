package adventofcode2024

import adventofcode2024.Day20.TrackObject.Start

import scala.annotation.tailrec

package object common {
  case class Coords(
    x: Int,
    y: Int,
  ) {
    def +(other: Coords): Coords = Coords(x + other.x, y + other.y)
    def *(scalar: Int): Coords = Coords(x * scalar, y * scalar)
    def -(other: Coords): Coords = this + (other * -1)
    def inverse: Coords = this * (-1)
    def isWithinBounds(dimX: Int, dimY: Int): Boolean = x >= 0 && x < dimX && y >= 0 && y < dimY
    def isWithinBounds(matrix: Vector[Vector[_]]): Boolean = isWithinBounds(matrix.size, matrix.head.size)
    def isWithinBoundingBox(lhs: Coords, rhs: Coords): Boolean = {
      math.min(lhs.x, rhs.x) <= x && x <= math.max(lhs.x, rhs.x) && math.min(lhs.y, rhs.y) <= y && y <= math.max(lhs.y, rhs.y)
    }
  }

  object Coords {
    def rangeInclusive(from: Coords, to: Coords): Vector[Coords] = {
      for {
        x <- from.x to to.x
        y <- from.y to to.y
      } yield Coords(x, y)
    }.toVector

    def rangeExclusive(from: Coords, to: Coords): Vector[Coords] = {
      for {
        x <- from.x until to.x
        y <- from.y until to.y
      } yield Coords(x, y)
    }.toVector

    val directions = Set(
      Coords(0,  1),
      Coords(1,  0),
      Coords(0, -1),
      Coords(-1, 0),
    )

    def manhattanDist(lhs: Coords, rhs: Coords): Long =
      math.abs(lhs.x - rhs.x) + math.abs(lhs.y - rhs.y)
  }

  def iterateWhile[T](init: T)(f: T => Option[T]): Vector[T] = {
    @tailrec
    def loop(cur: T, elemsSoFar: Vector[T]): Vector[T] = {
      f(cur) match {
        case Some(next) => loop(next, elemsSoFar :+ next)
        case None => elemsSoFar
      }
    }

    loop(init, Vector(init))
  }
  
  def fix[T](init: T)(f: T => T): T = {
    iterateWhile(init) { x => 
      val y = f(x)
      if (y == x) None else Some(y)
    }.last
  }

  extension [T](vv: Vector[Vector[T]]) {
    def updated(coords: Coords, value: T): Vector[Vector[T]] = {
      vv.updated(coords.x, vv(coords.x).updated(coords.y, value))
    }

    def at(coords: Coords): Option[T] = {
      if (coords.isWithinBounds(vv.size, vv.head.size)) {
        Some(vv(coords.x)(coords.y))
      } else {
        None
      }
    }

    def atUnsafe(coords: Coords): T = vv.at(coords).get

    def neighboursOf(coords: Coords): Set[Coords] = Coords.directions
      .map(dir => coords + dir)
      .filter(dest => dest.isWithinBounds(vv.size, vv.head.size))

    def findElem(pred: T => Boolean): Option[Coords] = {
      val x = Coords
        .rangeExclusive(Coords(0,0), Coords(vv.size, vv.head.size))

      x.find(pos => vv.at(pos).exists(pred))
    }
  }
  
  extension [K,V1, V2](m: Map[K,V1]) {
    def mapVals(f: V1 => V2): Map[K, V2] = m.view.mapValues(f).toMap
  }
}
