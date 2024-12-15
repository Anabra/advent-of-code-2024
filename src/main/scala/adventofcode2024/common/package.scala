package adventofcode2024

import scala.annotation.tailrec

package object common {
  case class Coords(
    x: Int,
    y: Int,
  ) {
    def +(other: Coords): Coords = Coords(x + other.x, y + other.y)
    def isWithinBounds(dimX: Int, dimY: Int): Boolean = x >= 0 && x < dimX && y >= 0 && y < dimY
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
  
  object VectorVectorExtensions {
    implicit class VectorVectorOps[T](val vv: Vector[Vector[T]]) extends AnyVal {
      def updated(coords: Coords, value: T): Vector[Vector[T]] = {
        vv.updated(coords.x, vv(coords.x).updated(coords.y, value))
      }
    }
  }
}
