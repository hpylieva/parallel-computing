package ua.edu.ucu.cs.parallel

import org.scalameter.{Key, Warmer, config}

import scala.util.Random

object MonteCarloPiEstimation {
  def pi(totalNumberOfPoints: Int): Double = 4.0 * countPointsInsideCircle(totalNumberOfPoints) / totalNumberOfPoints

  def countPointsInsideCircle(totalNumberOfPoints: Int): Int = {
    def rndX = new Random
    def rndY = new Random

    def simulate(hits: Int, pointsGenerated: Int): Int =
      if (pointsGenerated >= totalNumberOfPoints)
        hits
      else {
        val x = rndX.nextDouble
        val y = rndY.nextDouble
        simulate(hits + (if (x * x + y * y <= 1) 1 else 0), pointsGenerated + 1)
      }

    simulate(0, 0)
  }

  def piPar(totalNumberOfPoints: Int) = {
    val((pi1, pi2), (pi3, pi4)) = parallel(parallel(countPointsInsideCircle(totalNumberOfPoints/4), countPointsInsideCircle(totalNumberOfPoints/4)),
      parallel(countPointsInsideCircle(totalNumberOfPoints/4), countPointsInsideCircle(totalNumberOfPoints/4)))

    4.0*(pi1 + pi2 + pi3 + pi4) / totalNumberOfPoints
  }

  def main(args: Array[String]): Unit = {

    val totalNumberOfPoints = 100000

    //println(pi(totalNumberOfPoints))

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true) withWarmer new Warmer.Default

    val seqtime = standardConfig measure {
      pi(totalNumberOfPoints)
    }

    val partime = standardConfig measure {
      piPar(totalNumberOfPoints)
    }

    println(s"sequential time $seqtime ms")
    println(s"parallel time $partime ms")
    println(s"speedup: ${seqtime.value/partime.value}")
  }
}


