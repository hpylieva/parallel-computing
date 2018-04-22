package ua.edu.ucu.cs.parallel

import org.scalameter._

import scala.math._
import scala.util.Random

object MonteCarloNumericalIntegration {

  def findMinAndMax(f:Double => Double, xmin: Double, xmax: Double, step: Double = 0.0001): (Double, Double) = {

    def find(ymin:Double, ymax: Double, steps: Int): (Double, Double) = {
      if (steps >= math.round((xmax-xmin)/step)) (ymin, ymax)
      else {
        val x = xmin + step * steps
        val y = f(x)
        find(min(y, ymin), max(y, ymax), steps+1)
      }
    }
   find(f(xmin), f(xmin), 0)

  }

  def integrate(f:Double => Double, xmin: Double, xmax: Double, totalNumberOfPoints: Int): Double = {
    val (ymin, ymax) = findMinAndMax(f, xmin, xmax)
    val rectArea = (ymax - ymin)*(xmax - xmin)

    rectArea * countPointsUnderCurve(f, xmin, xmax, ymin, ymax, totalNumberOfPoints) / totalNumberOfPoints
  }

  def countPointsUnderCurve(f:Double => Double, xmin: Double, xmax: Double, ymin: Double, ymax: Double, totalNumberOfPoints: Int): Int = {
    def rndX = new Random
    def rndY = new Random

    def simulate(hits: Int, pointsGenerated: Int): Int =
      if (pointsGenerated >= totalNumberOfPoints)
        hits
      else {
        val x = xmin + (xmax - xmin)*rndX.nextDouble
        val y = ymin + (ymax - ymin)*rndY.nextDouble
        val stepUpdate: Int = {
          if (abs(y) <= abs(f(x))) {
            if (f(x) > 0 & y > 0 & y <= f(x)) 1
            else {
              if (f(x) < 0 & y < 0 & y >= f(x)) -1
              else 0
            }
          } else 0
        }
        simulate(hits + stepUpdate, pointsGenerated + 1)
      }

    simulate(0, 0)
  }

  def integratePar(f:Double => Double, xmin: Double, xmax: Double, totalNumberOfPoints: Int) = {
    val (ymin, ymax) = findMinAndMax(f, xmin, xmax)
    val rectArea = (ymax - ymin)*(xmax - xmin)

    val((i1, i2), (i3, i4)) = parallel(
      parallel(countPointsUnderCurve(f, xmin, xmax, ymin, ymax, totalNumberOfPoints/4),
              countPointsUnderCurve(f, xmin, xmax, ymin, ymax, totalNumberOfPoints/4)),
      parallel(countPointsUnderCurve(f, xmin, xmax, ymin, ymax, totalNumberOfPoints/4),
              countPointsUnderCurve(f, xmin, xmax, ymin, ymax, totalNumberOfPoints/4)))

    rectArea*(i1 + i2 + i3 + i4) / totalNumberOfPoints

  }

  def main(args: Array[String]): Unit = {

    val totalNumberOfPoints = 100000
    def f(x: Double): Double = sin(x)
    val xmin = 0
    val xmax = Pi
    println(integrate(f, xmin, xmax, totalNumberOfPoints))


    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true) withWarmer new Warmer.Default

    val seqtime = standardConfig measure {
      integrate(f, xmin, xmax, totalNumberOfPoints)
    }

    val partime = standardConfig measure {
      integratePar(f, xmin, xmax, totalNumberOfPoints)
    }

    println(s"sequential time $seqtime ms")
    println(s"parallel time $partime ms")
    println(s"speedup: ${seqtime.value/partime.value}")
  }

}


