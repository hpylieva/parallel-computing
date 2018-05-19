package ua.edu.ucu.cs.parallel

import scala.io.Source

object WordCount3{

  trait Monoid[A]{
    def op(x: A, y: A): A
    def zero: A
  }


  case class LineCounter(leftUnfinished: Boolean, count: Int, rightUnfinished: Boolean)
  case class FullWords(leftPart: String, countWords: Int, rightPart: String)
  //Parallel folding with mapping
  def foldMapPar[A, B](xs: IndexedSeq[A],
                       from: Int, to: Int, m: Monoid[B])(f: A => B)
                      (implicit thresholdSize: Int): B =
    if (to-from < thresholdSize)
      foldMapSegment(xs, from, to, m)(f)
    else {
      val middle = from + (to - from) / 2
      val (l, r) = parallel(
        foldMapPar(xs, from, middle, m)(f)(thresholdSize),
        foldMapPar(xs, middle, to, m)(f)(thresholdSize))
      m.op(l, r)
    }

  def foldMapSegment[A,B](xs: IndexedSeq[A],from: Int, to: Int,
                          m: Monoid[B])(f: A => B): B = {
    var res = f(xs(from))
    var index = from + 1
    while (index < to){
      print(m.op(res, f(xs(index))))
      res = m.op(res, f(xs(index)))
      index = index + 1
    }
    res
  }

  def getLinesFromFile(filename: String) = {
    val lines = Source.fromFile(filename).getLines.mkString(" ")
    //    println(s"The file $filename contains ${lines.length} lines.")
    lines
  }

  val wordCountMonoid =  new Monoid[(String, Int, String)] {
    override def op(x: (String, Int, String), y: (String, Int, String)) =
      (x._1, x._2 + y._2 + 1, y._3)
    override def zero = ("", 0, "")
  }

  def f(c: Char): (String, Int, String) = c match {
    case ' ' => ("", 1, "")
    case c:Char => (c.toString,0, "")

  }

  private def combine(x: (String, Int, String)): Int = x._2 + 2 //(if (x.leftUnfinished) 1 else 2) + (if (x.rightUnfinished) 1 else 0)

  def wordsCounterSeq(source: String): Int = {
    combine(foldMapSegment[Char, (String, Int, String)](source.toIndexedSeq, 0, source.length, wordCountMonoid)(f))
  }

  def wordsCounterPar(source: String): Int = {
    implicit val threshold: Int = 10
    combine(foldMapPar[Char, (String, Int, String)](source.toIndexedSeq,  0, source.length, wordCountMonoid)(f))
  }

  def main(args: Array[String]): Unit = {
    val source = getLinesFromFile("src/main/scala/WordCount/small.txt")
    println(s"Count sequential:   ${wordsCounterSeq(source)}")
    println(s"Count parallel:     ${wordsCounterPar(source)}")
//    println("*** Speed Measures ***")

//    val standardConfig = config(
//      Key.exec.minWarmupRuns -> 5,
//      Key.exec.maxWarmupRuns -> 10,
//      Key.exec.benchRuns -> 100,
//      Key.verbose -> true
//    ).withWarmer(new Warmer.Default)
//    val seqTime = standardConfig.measure(wordsCounterSeq(source))
//    val parTime = standardConfig.measure(wordsCounter(source))
//
//    println(s"sequential time:  $seqTime")
//    println(s"parallel time:    $parTime")
//    println(s"speedup           ${seqTime.value / parTime.value}")
  }

}