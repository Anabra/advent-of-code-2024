package adventofcode2024

import adventofcode2024.common.*
import adventofcode2024.common.graphs.*

import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable.Set as rhs
import scala.collection.mutable

object Day23 {
  def main(args: Array[String]): Unit = {
    println(task1())
    println(task2())
  }

  type ComputerId = String
  type Network = Map[ComputerId, Set[ComputerId]]

  def readInput(path: String): Network = {
    val bufferedSource = io.Source.fromResource(path)
    val lines = bufferedSource.getLines.toVector
    bufferedSource.close

    val edges = lines.map(_.split("-")).map { case Array(lhs, rhs) => (lhs, rhs) }

    edges.foldLeft(Map.empty[ComputerId, Set[ComputerId]]) { case (network, (lhs, rhs)) =>
      network
        .updatedWith(lhs) {
          case None => Some(Set(rhs))
          case Some(neighbours) => Some(neighbours + rhs)
        }
        .updatedWith(rhs) {
          case None => Some(Set(lhs))
          case Some(neighbours) => Some(neighbours + lhs)
        }
    }
  }

  def calcClusters(network: Network, computer: ComputerId): Set[Set[ComputerId]] = {
    val neighbours = network(computer)

    @tailrec
    def loop(clustersSoFar: Set[Set[ComputerId]], neighboursLeft: Vector[ComputerId]): Set[Set[ComputerId]] = {
      neighboursLeft match {
        case Vector(next, rest*) =>
          val neighboursOfNext = network(next)
          val (fullyConnectedNeighbours, newRest) = rest.partition(neighboursOfNext.contains)
          val newCluster = fullyConnectedNeighbours.toSet + computer + next
          loop(clustersSoFar + newCluster, newRest.toVector)
        case Vector() =>
          clustersSoFar
      }
    }

    loop(Set.empty, neighbours.toVector)
  }

  def calcClustersWithSIzeNPlusOne(network: Network, clustersWithSizeN: Set[Set[ComputerId]]): Set[Set[ComputerId]] = {
    clustersWithSizeN.flatMap { cluster =>
      val commonNeighboursOfEveryone = cluster.map(n => network(n)).reduce(_ intersect _)
      val newClusters = commonNeighboursOfEveryone.map(cluster + _)
      newClusters
    }
  }

  def calcAllClusters(network: Network): Map[Int, Set[Set[ComputerId]]] = {
    val duoClusters = network.flatMap { case (computer, neighbours) => neighbours.map(n => Set(n, computer)) }.toSet
    val progressivelyBiggerClusters = iterateWhile(duoClusters) { clustersWithSizeN =>
      val biggerClusters = calcClustersWithSIzeNPlusOne(network, clustersWithSizeN)
      if (biggerClusters.isEmpty) None else Some(biggerClusters)
    }
    (2 to network.size).zip(progressivelyBiggerClusters).toMap
  }

  def task1(): Int = {
    val network = readInput("day23.txt")
    val clusters = calcAllClusters(network)

    clusters(3).count(_.exists(id => id.startsWith("t")))
  }

  def task2(): String = {
    val network = readInput("day23.txt")
    val clusters = calcAllClusters(network)
    val maxSize = clusters.keySet.max

    clusters(maxSize).head.toVector.sorted.mkString(",")
  }
}
// 1349 too hi
