package ua.edu.cs.parallel

import java.util.concurrent.{ForkJoinPool, ForkJoinWorkerThread, RecursiveTask}

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

  def piPair(totalNumberOfPoints: Int) = {
    val(pi1, pi2) = parallel(countPointsInsideCircle(totalNumberOfPoints/2), countPointsInsideCircle(totalNumberOfPoints/2))
    4.0*(pi1 + pi2) / totalNumberOfPoints
  }

//  def main(args: Array[String]): Unit = {
//    val totalNumberOfPoints = 1000000
//    val standardConfig = config(
//      Key.exec.minWarmupRuns -> 100,
//      Key.exec.maxWarmupRuns -> 300,
//
//    )
//  }

  val forkJoinPool = new ForkJoinPool

  def task[T](computation: => T): RecursiveTask[T] = {
    val t = new RecursiveTask[T] {
      def compute = computation
    }

    Thread.currentThread match {
      case wt: ForkJoinWorkerThread =>
        t.fork() // schedule for execution
      case _ =>
        forkJoinPool.execute(t)
    }

    t
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    val right = task { taskB }
    val left = taskA

    (left, right.join())
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }
}


