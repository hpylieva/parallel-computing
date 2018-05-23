package ua.edu.ucu.cs.parallel

import java.io.{BufferedWriter, File, FileWriter}

import org.scalameter._

object QuickSort{
  def quickSortTraditional(xs: Array[Int]) {
    def swap(i: Int, j: Int) {
      val t = xs(i); xs(i) = xs(j); xs(j) = t
    }
    def sort(l: Int, r: Int) {
      val pivot = xs((l + r) / 2)
      var i = l; var j = r
      while (i <= j) {
        while (xs(i) < pivot) i += 1
        while (xs(j) > pivot) j -= 1
        if (i <= j) {
          swap(i, j)
          i += 1
          j -= 1
        }
      }
      if (l < j) sort(l, j)
      if (j < r) sort(i, r)
    }
    sort(0, xs.length - 1)
  }


  def quickSortFunctional(list: List[Int]): List[Int] = {
    list match {
      case Nil        => Nil
      case a :: tail  =>
        val (less,greater) = tail.partition(_ <= a)
        quickSortFunctional(less) ::: List(a) ::: quickSortFunctional(greater)
    }
  }

  def quickSortParNaive(xs: Array[Int]): Unit ={
    parallel(quickSortTraditional(xs.slice(0, xs.length/2)),
      quickSortTraditional(xs.slice(xs.length/2, xs.length)))
  }

  def writeToFile(input: List[Int], filename: String ):Unit = {
    val file = new File("src/main/scala/QuickSort/"+filename)
    val input_txt = input.mkString("\n")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(input_txt)
    bw.close()
  }

  def main(args: Array[String]): Unit = {
    val r = scala.util.Random
    val max = 10000
    val arr = for (i <- 1 to max) yield r.nextInt(max * 2)
    writeToFile(arr.toList, "unsorted.txt")

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 10,
      Key.exec.maxWarmupRuns -> 50,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ).withWarmer(new Warmer.Default)
//    println("COMPARE",quickSortFunctional(arr.toList)==arr.sorted.toList)
    val builtInScalaTime = standardConfig.measure(
      scala.util.Sorting.quickSort(arr.toArray))
    val tradTime = standardConfig.measure(quickSortTraditional(arr.toArray))
    val funcTime = standardConfig.measure(quickSortFunctional(arr.toList))
    val parNaiveTime = standardConfig.measure(quickSortParNaive(arr.toArray))

    println(s"Scala built-in time:  $builtInScalaTime")
    println(s"traditional approach time:  $tradTime")
    println(s"functional approach time:    $funcTime")
    println(s"parallel naive approach time:    $parNaiveTime")
    println(s"speedup           ${tradTime.value / funcTime.value}")
    println("functional approach is slower. This happens because the traditional operates " +
      "in-place on the original array, so no copies are done and no additional memory is needed." +
      " The functional one allocates a new list and copies a lot of data on each call. ")


  }
}
