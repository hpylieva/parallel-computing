package ua.edu.ucu.cs.parallel
import org.scalameter

import scala.util.Random
import org.scalameter._

object SimpleParallelComputations{
  def main(args: Array[String]): Unit = {

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 1000,
      Key.exec.benchRuns -> 100,
      Key.verbose -> false // to see all the measurements in the end
    ) withWarmer new scalameter.Warmer.Default

    def maxSegment(array: Array[Int], from: Int, to: Int): Int = {
      def iter(max: Int, index: Int): Int =
        if (index < to)
          iter( if (array(index)>max) array(index) else max,
            index +1)
        else max
      iter(Int.MinValue, from)
    }

    def sumSegment(array: Array[Int], from: Int, to: Int): Int = {
      def iter(sum: Int, index: Int): Int =
        if (index < to) iter(sum + array(index), index +1)
        else sum
      iter(0, from)
    }

    val random = new Random
    val array = (0 until 10000000).map(x => random.nextInt(10)).toArray

    val seqtime = standardConfig measure{
      val (max, sum) = parallel(
        maxSegment(array, 0, array.length),
        sumSegment(array, 0, array.length)
      )
//      println(s"max = $max")
//      println(s"sum = $sum")
    }

    val middle = array.length/2
    // sequentially 2 parallel computings
    // between those 2 parallel co,putings we split the segments of memory
    val partime = standardConfig measure{
     val (maxLeft, sumRight) = parallel(
        maxSegment(array, 0, middle),
        sumSegment(array, middle, array.length)
      )
      val (sumLeft, maxRight) = parallel(
        sumSegment(array, 0, middle),
        maxSegment(array, middle, array.length)
      )
      val max = if (maxLeft > maxRight) maxLeft else maxRight
      val sum = sumLeft + sumRight
//      println(s"max = $max")
//      println(s"sum = $sum")
    }

    println(s"seqtime = $seqtime")
    println(s"partime = $partime")
    val speedup = seqtime.value / partime.value
    println(s"speedup = $speedup")

  }
}
