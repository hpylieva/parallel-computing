package algorithms.parallel

import scala.math.{floor, log}
import scala.util.Random
import akka.actor._


object MergeSort2 extends App {

  case class Items(items: Vector[Int])
  // The Worker Actor takes items to sort, and its height
  // in the worker tree. If it's the edge of the tree, it
  // sorts the items, and sends them back to the parent
  // worker. Otherwise, it splits the items in half, and
  // passes each half to a child worker.
  class Worker(items: Vector[Int], height: Int) extends Actor {
    if (height > 1) {
      items.grouped(items.length / 2) foreach {items =>
        context.actorOf(Props(new Worker(items, height - 1)))
      }
    } else {
      context.parent ! Items(sort(items))
    }

    def receive = {

      case Items(first) =>
        context.become {
          case Items(second) =>
            context.parent ! Items(merge(first, second))
            context.stop(self)
        }
    }
  }

  // The Runner Actor takes the initial run args and starts the
  // root worker. It calculates the height of the worker tree
  // so that workers will know when they're at the edge and
  // must sort. Once the sorted items are received, it either
  // starts a new Runner, or shuts down entirely.
  class Runner(size: Int, workers: Int, runs: Int) extends Actor {

    val unsorted = Vector.fill(size)(Random.nextInt(maxInt))
    val height = floor(log(workers) / log(2)).toInt
    val start = System.nanoTime

    println(s"Let's sort the next: $unsorted")
    context.actorOf(Props(new Worker(unsorted, height)))

    def receive = {
      case Items(items) =>
        val end = (System.nanoTime - start) / 1e7
        if (unsorted.sorted.mkString != items.mkString) {
          println("Invalid sort")
        } else {
          println(s"Sorted $size items with $workers workers in $end msecs")
          println(s"Final result: $items\n")
        }
        if (runs == 1) {
          context.system.shutdown()
        } else {
          context.system.actorOf(Props(new Runner(size, workers, runs - 1)))
          context.stop(self)
        }
    }

  }

  def merge(left: Vector[Int], right: Vector[Int]): Vector[Int] = {
    println(s"$left and $right")
    var leftIndex = 0
    var rightIndex = 0
    var merged = Vector[Int]()
    while (leftIndex < left.length && rightIndex < right.length) {
      if (left(leftIndex) <= right(rightIndex)) {
        merged :+= left(leftIndex)
        leftIndex += 1
      } else {
        merged :+= right(rightIndex)
        rightIndex += 1
      }
    }
    if (leftIndex == left.length) {
      merged ++ right.slice(rightIndex, right.length)
    } else {
      merged ++ left.slice(leftIndex, left.length)
    }
  }

  def sort(items: Vector[Int]): Vector[Int] = {
    items match {
      case Vector(_) => items
      case _ =>
        val (left, right) = items.splitAt(items.length / 2)
        merge(sort(left), sort(right))
    }
  }

  val system = ActorSystem()
  val maxInt = 100
  system.actorOf(Props(new Runner(size = 16, workers = 4, runs = 1)))
  system.awaitTermination()

}