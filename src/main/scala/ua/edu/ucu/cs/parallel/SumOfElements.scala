package ua.edu.ucu.cs.parallel

import org.scalameter._

import scala.util.Random

object SumOfElements{

  def power(x: Int, p: Double): Int =
    math.exp(p * math.log(Math.abs(x))).toInt

  def sumSegment(a: Array[Int], p: Double, from: Int, to: Int): Int = {
    def iter(sum: Int, index: Int): Int =
      if (index >= to) sum
      else iter(sum + a(index), index +1)

    iter(0, from)
  }

  var threshold = 10000 /// by reducing the threshold we are producing more parallel computations

  def sumSegmentPar(a: Array[Int], p: Double, from: Int, to: Int): Int = {
    if (to - from < threshold)
      sumSegment(a, p, from, to)
    else {
      val middle = from + (to - from)/2
      val (sum1, sum2) = parallel(sumSegmentPar(a, p, from, middle), sumSegmentPar(a,p,middle, to))
      sum1+sum2
    }
  }

  def sumOfElements(a: Array[Int], p: Double): Int = sumSegment(a,p,0,a.length)

  def sumOfElementsPar(a: Array[Int], p: Double): Int = sumSegmentPar(a,p,0, a.length)

  def main(args: Array[String]): Unit = {
    val rnd = new Random
    val length = 1000000
    val input = (0 until length).map(_ * rnd.nextInt()).toArray

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true) withWarmer (new Warmer.Default)

    val seqtime = standardConfig measure {
      sumOfElements(input, 2)
    }

    val partime = standardConfig measure {
      sumOfElementsPar(input, 2)
    }

    println(s"sequential time $seqtime ms")
    println(s"parallel time $partime ms")
    println(s"speedup: ${seqtime.value/partime.value}")
  }
}
