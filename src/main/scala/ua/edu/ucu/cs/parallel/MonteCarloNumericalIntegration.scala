package ua.edu.ucu.cs.parallel

import org.scalameter._

import scala.math._
import scala.util.Random

object MonteCarloNumericalIntegration {

  def evaluateSum(f:Double => Double, xmin: Double, xmax: Double, totalNumberOfPoints: Int,
                  cumsum: Double, pointsGenerated: Int): Double = {
    if (pointsGenerated >= totalNumberOfPoints)
      cumsum
    else {
      val random = new Random
      val rndX = xmin + random.nextDouble() * (xmax - xmin)
      val rectArea = f(rndX) * (xmax - xmin)
      evaluateSum(f,xmin, xmax, totalNumberOfPoints, cumsum + rectArea, pointsGenerated + 1)
    }
  }

  def integrateSeq(f:Double => Double, xmin: Double, xmax: Double, totalNumberOfPoints: Int): Double = {
    val sumRectArea = evaluateSum(f,xmin, xmax, totalNumberOfPoints, 0, 0)
    sumRectArea/totalNumberOfPoints
  }


  def integratePar(f:Double => Double, xmin: Double, xmax: Double, totalNumberOfPoints: Int): Double = {
    val(i1, i2, i3, i4) = parallel(
      integrateSeq(f,xmin, xmax, totalNumberOfPoints/4),
      integrateSeq(f,xmin, xmax, totalNumberOfPoints/4),
      integrateSeq(f,xmin, xmax, totalNumberOfPoints/4),
      integrateSeq(f,xmin, xmax, totalNumberOfPoints/4))

    (i1 + i2 + i3 + i4) / 4

  }

  def main(args: Array[String]): Unit = {

    val totalNumberOfPoints = 100000
    def f(x: Double): Double = sin(x)
    val xmin = 0
    val xmax = Pi
//    println(integrateSeq(f, xmin, xmax, totalNumberOfPoints))
//    println(integratePar(f, xmin, xmax, totalNumberOfPoints))

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 1000,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true) withWarmer new Warmer.Default

    val seqtime = standardConfig measure {
      integrateSeq(f, xmin, xmax, totalNumberOfPoints)
    }

    val partime = standardConfig measure {
      integratePar(f, xmin, xmax, totalNumberOfPoints)
    }

    println(s"sequential time $seqtime ms")
    println(s"parallel time $partime ms")
    println(s"speedup: ${seqtime.value/partime.value}")
  }

}