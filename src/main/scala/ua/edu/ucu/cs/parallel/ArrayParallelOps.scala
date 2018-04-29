package ua.edu.ucu.cs.parallel

import org.scalameter.{Key, Warmer, config}

import scala.util.Random

object ArrayParallelOps{
  def power(x: Int, p: Double): Int = math.exp(p * math.log(Math.abs(x))).toInt

  def mapArraySegmentSeq[A,B](
       source: Array[A],
       from: Int, to:Int,
       f: A => B,
       target: Array[B]): Unit ={

    var index = from
    while (index < to) {
      target(index) = f(source(index))
      index = index + 1
    }
  }

  val threshold = 1000

  def mapArraySegmentPar[A,B](
       source: Array[A],
       from: Int, to:Int,
       f: A => B,
       target: Array[B]): Unit ={

    if (to - from < threshold) {
      mapArraySegmentSeq(source, from, to, f, target)
    } else {
      val middle = from + (to - from) / 2
      parallel(mapArraySegmentPar(source, from, middle, f, target),
        mapArraySegmentPar(source, middle, to, f, target))
    }
  }

  def raiseArrayToPower(
       source: Array[Int],
       from: Int, to: Int,
       p: Double,
       target: Array[Double]): Unit = {
    if (to - from < threshold){
      var index = from
      while (index < to) {
        target(index) = power(source(index), p)
        index = index + 1
      }
    } else {
        val middle = from + (to - from) / 2
        parallel(raiseArrayToPower(source, from, middle, p, target),
          raiseArrayToPower(source, middle, to, p, target))
      }
  }

  def main(args: Array[String]): Unit = {
    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 2800,
      Key.exec.benchRuns -> 100,
      Key.verbose -> false) withWarmer(new Warmer.Default)

    var rnd = new Random
    val length = 1000000
    val source = (0 until length).map(_ * rnd.nextInt()).toArray
    val target = Array.fill(length){0.0}
    val p = 1.3
    val f: Int => Double = x => power(x, p)

    println("Sequential computation:...")
    val seqtime = standardConfig measure{
      mapArraySegmentSeq(source, 0, length, f, target)
    }
    println(s" completed in $seqtime")

    println("Parallel computation:...")
    val partime = standardConfig measure{
      mapArraySegmentPar(source, 0, length, f, target)
    }
    println(s" completed in $partime")

    println("Special parallel computation:...")
    val partimeSpec = standardConfig measure{
      raiseArrayToPower(source, 0, length, p, target)
    }
    println(s" completed in $partimeSpec\n\n")

    println(s"speedup seq map vs par map: ${seqtime.value/partime.value}")
    println(s"speedup par map vs spec par map: ${partime.value/partimeSpec.value}")
  }
}