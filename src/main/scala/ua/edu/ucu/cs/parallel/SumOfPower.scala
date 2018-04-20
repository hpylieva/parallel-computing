import org.scalameter._

import scala.util.Random

object SumOfPower{

  def power(x: Int, p: Double): Int =
    math.exp(p * math.log(Math.abs(x))).toInt

  def sumSegment(a: Array[Int], p: Double,
                 from: Int, to: Int): Int = {
    def iter(sum: Int, index: Int): Int =
      if (index >= to) sum
      else iter(sum + power(a(index),p), index +1)

    iter(0, from)
  }

  def pNorm(a: Array[Int], p: Double): Int =
    power(sumSegment(a,p,0,a.length), 1/p)


  def main(args: Array[String]): Unit = {
    val rnd = new Random
    val length = 100000
    val input = (0 until length).map(_ * rnd.nextInt()).toArray

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true) withWarmer(new Warmer.Default)

    val seqtime = standardConfig measure {
      pNorm(input, 2)
    }

    val partime = standardConfig measure {
      //TODO: change to parallel
      pNorm(input, 2)
    }

    println(s"sequential time $seqtime ms")
    println(s"parallel time $partime ms")
    println(s"speedup: ${seqtime.value/partime.value}")
//    println(pNorm(input, 2))
  }
}
