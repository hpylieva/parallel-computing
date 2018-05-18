package ua.edu.ucu.cs.parallel

import scala.io.Source

object WordCount2{

  trait Monoid[A]{
    def op(x: A, y: A): A
    def zero: A
  }

  case class LineCounter(leftUnfinished: Boolean, count: Int, rightUnfinished: Boolean)

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

  def readFile(filename: String): Vector[String] = {
    val bufferedSource = Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines) yield line).toVector
    bufferedSource.close()
    println(s"number of lines in $filename: ${lines.length}")
    lines
  }

  def lineWordsCounter(line: String): LineCounter = {
    val regex = "[\\.,\\s!\\?;:()\\\"]+"
    val artEndOfLine = "|#|"
    val splitted = (line + "|#|").split(regex)
    val fullWordsLength = splitted.length - 2
    val leftPunctuation = splitted.head == ""
    val rightPunctuation = splitted.last == artEndOfLine

    println(splitted.head, splitted.length -2 , splitted.last)

    LineCounter(!leftPunctuation, fullWordsLength, !rightPunctuation)
  }

  def makeMonoid(): Monoid[LineCounter] = new Monoid[LineCounter] {
    def op(x: LineCounter, y: LineCounter): LineCounter = {
      val unfinishedBetween = x.rightUnfinished && y.leftUnfinished
      LineCounter(x.leftUnfinished, x.count + y.count + (if (unfinishedBetween) 1 else 2), y.rightUnfinished)
    }

    def zero: LineCounter = LineCounter(leftUnfinished = false, 0, rightUnfinished = false)
  }

  private def combine(x: LineCounter): Int = x.count + 2 //(if (x.leftUnfinished) 1 else 2) + (if (x.rightUnfinished) 1 else 0)

  def wordsCounterSeq(source: Vector[String]): Int = {
    val monoid = makeMonoid()
    combine(foldMapSegment[String, LineCounter](source, from = 0, source.length, monoid)(lineWordsCounter))
  }

  def wordsCounter(source: Vector[String]): Int = {
    implicit val threshold: Int = 10
    val monoid = makeMonoid()
    combine(foldMapPar[String, LineCounter](source, from = 0, source.length, monoid)(lineWordsCounter))
  }

  def main(args: Array[String]): Unit = {
    val source = readFile("src/main/scala/WordCount/small.txt")
    println(s"Count sequential:   ${wordsCounterSeq(source)}")
    println(s"Count parallel:     ${wordsCounter(source)}")
    println("*** Speed Measures ***")

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