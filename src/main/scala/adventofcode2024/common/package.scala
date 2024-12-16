package adventofcode2024

import scala.annotation.tailrec

package object common {
  case class Coords(
    x: Int,
    y: Int,
  ) {
    def +(other: Coords): Coords = Coords(x + other.x, y + other.y)
    def *(scalar: Int): Coords = Coords(x * scalar, y * scalar)
    def -(other: Coords): Coords = this + (other * -1)
    def isWithinBounds(dimX: Int, dimY: Int): Boolean = x >= 0 && x < dimX && y >= 0 && y < dimY
    def isWithinBoundingBox(topLeft: Coords, bottomRight: Coords): Boolean = {
      topLeft.x <= x && x <= bottomRight.x && topLeft.y <= y && y <= bottomRight.y
    }
  }
  
  object Coords {
    def rangeInclusive(from: Coords, to: Coords): Vector[Coords] = {
      for {
        x <- from.x to to.x
        y <- from.y to to.y
      } yield Coords(x, y)
    }.toVector
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
  }
}
