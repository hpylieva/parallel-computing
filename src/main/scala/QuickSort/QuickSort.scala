package ua.edu.ucu.cs.parallel

import java.io.{BufferedWriter, File, FileWriter}

import org.scalameter._

import scala.util.Random

object QuickSort{
  def quickSortTraditional(xs: Array[Int]) {
    // uses always the central element as a pivot
    // goes from both sides to to central element and checks if
    // left and right sides contain the 'right' elements
    def swap(i: Int, j: Int) {
      val t = xs(i); xs(i) = xs(j); xs(j) = t
    }
    def qsort(l: Int, r: Int) {
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
      if (l < j) qsort(l, j)
      if (j < r) qsort(i, r)
    }
    qsort(0, xs.length - 1)
  }


  def quickSortFunctional(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case a :: tail  =>
        val (less,greater) = tail.partition(_ <= a)
        quickSortFunctional(less) ::: List(a) ::: quickSortFunctional(greater)
    }
  }

  def quickSortPar(xs: Array[Int]): Array[Int] = {
    // uses always the left element as a pivot
    val <= = (a:Int, b: Int) => a <= b
    val > = (a:Int, b: Int) => a > b

    def getPart(list: Array[Int], pivot: Int, fn: (Int, Int) => Boolean): Array[Int] = {
      // marking elements <= pivot as 1 else 0
//      def findF(f: Array[Int], current: Int): Array[Int] = {
//        if (f(current) <= pivot) f(current) = 1 //else f(current) = 0
//        findF(f, current+1)
//      }
//      val F: Array[Int] = findF(new Array[Int](list.length), 0)
      def f(x: Int) = if (fn(x, pivot)) 1 else 0
      val F = list.map( x => f(x))
      // prefix scan of Array F
      val K = F.scanLeft(0)((acc, elem) => acc + elem).drop(1)
      val L = F.zipWithIndex.par.flatMap{case (x, id) => if (x==1) List(list(id)) else Nil }
      L.toArray
    }

    def qsortPar(list: Array[Int]): Array[Int] = {
      val pivot = list(rand.nextInt(list.length))
      val (l,r) = parallel(getPart(list, pivot, <=),  getPart(list, pivot, >))
      if (printParallelProgress)
        println(s"l: [${l.mkString(" ")}] r: [${r.mkString(" ")}]")
      val al = if (l.length == 1 || l.forall(_ == l.head)) l else qsortPar(l)
      val ar = if (r.length == 1 || r.forall(_ == r.head)) r else qsortPar(r)
      al ++ ar
    }
    qsortPar(xs)
  }

  def writeToFile(input: List[Int], filename: String ):Unit = {
    val file = new File("src/main/scala/QuickSort/"+filename)
    val input_txt = input.mkString("\n")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(input_txt)
    bw.close()
  }

  val rand = new Random(seed = 42)
  // do not change printParallelProgress when measuring performance of algorithms
  // this will affect parTime a lot because of printing
  var printParallelProgress = false

  def main(args: Array[String]): Unit = {
    val max = 100000
    val arrRandom = (0 until max).map(_ => rand.nextInt(max * 2))
    writeToFile(arrRandom.toList, "unsorted.txt")

    val arrHardcoded = Array(12,8,1,1,5,13,2,4,9,3,15,6,7,11,10,14,16)

    val measurePerformance = true
    if (measurePerformance) {
      val standardConfig = config(
        Key.exec.minWarmupRuns -> 10,
        Key.exec.maxWarmupRuns -> 30,
        Key.exec.benchRuns -> 50,
        // in case details of measurements are important set Key.verbose -> true
        Key.verbose -> false
      ).withWarmer(new Warmer.Default)

      val builtInScalaTime = standardConfig.measure(
        scala.util.Sorting.quickSort(arrRandom.toArray))
      val tradTime = standardConfig.measure(quickSortTraditional(arrRandom.toArray))
      val funcTime = standardConfig.measure(quickSortFunctional(arrRandom.toList))
      val parTime = standardConfig.measure(quickSortPar(arrHardcoded))

      println("=================== Results ==================")
      println(s"Size of input array: $max \n")
      println(s"Scala built-in time:  $builtInScalaTime")
      println(s"traditional approach time:  $tradTime")
      println(s"functional approach time:    $funcTime")
      println(s"speedup of traditional compared to functional ${tradTime.value / funcTime.value}")
      println("\nFunctional approach is slower. This happens because the traditional operates\n " +
        "in-place on the original array, so no copies are done and no additional memory\n is needed." +
        " The functional one allocates a new list and copies a lot of data on each call. \n")


      println(s"parallel approach time:    $parTime")
      println(s"speedup of parallel compared to traditional ${tradTime.value / parTime.value}")
      println(s"speedup of parallel compared to functional ${funcTime.value / parTime.value}")
      println("\nAgain, when we compare parallel version Scala implementation with traditional,\n" +
        "trad is fasted fot the same reason explained above. It is more 'honest' to compare \n" +
        "parallel approach with functional. In this case we can see a great speedup.\n" +
        "Note that when size of array is e.g. 100 000, parallel approach becomes faster than even traditional."
      )
    }

    // to review how Parallel QuickSort works set reviewParallelWork = true
    val reviewParallelWork = false
    if (reviewParallelWork) {
      printParallelProgress = true
      println(s"Result of Parallel Sorting: \n${quickSortPar(arrRandom.toArray).mkString(" ")}")
    }
  }
}
