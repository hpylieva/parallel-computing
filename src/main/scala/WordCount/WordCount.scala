package ua.edu.ucu.cs.parallel

import org.scalameter._

import scala.io.Source

object WordCount{

  trait Monoid[A]{
    def op(x: A, y: A): A
    def zero: A
  }

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
//      println(res)
      index = index + 1
    }
    res
  }

  // read file where lines are separated by space
  def readFile(filename: String): String= {
    val lines = Source.fromFile(filename).getLines.mkString(" ")
    lines
  }

  sealed trait WordCount
  case class Stub(chars: String) extends WordCount
  case class Part(lStub: String, words: Int, rStub: String) extends WordCount

  val wordCountMonoid = new Monoid[WordCount] {
    override def op(a1: WordCount, a2: WordCount): WordCount = (a1, a2) match {
      case (Stub(s1), Stub(s2)) =>
        Stub(s1 + s2)
      case (Stub(s1), Part(ls2, w2, rs2)) =>
        Part(s1 + ls2, w2, rs2)
      case (Part(ls1, w1, rs1), Stub(s2)) =>
        Part(ls1, w1, rs1 + s2)
      case (Part(ls1, w1, rs1), Part(ls2, w2, rs2)) =>
        Part(ls1, w1 + w2 + (if ((rs1 + ls2).isEmpty) 0 else 1), rs2)
    }
    override val zero = Stub("")
  }

  def wordCountFunction(c: Char): WordCount = {
    // check for all signs separately not to count cases like "hello ! " as 2 words
    // the less signs in the stop-signs set - the bigger speedup of parallel version
    if (" ,.()[]{}!?" contains c) Part("", 0, "") else Stub(c.toString)
  }

  def unstub(s: String): Int = {
    s.length min 1
  }

  def wordCountSeq(sentence: String): Int =
    foldMapSegment(sentence, 0, sentence.length, wordCountMonoid)(wordCountFunction) match {
      case Stub(s) => unstub(s)
      case Part(l, words, r) => unstub(l) + words + unstub(r)
    }

  implicit val threshold: Int = 100
  def wordCountPar(sentence: String): Int = {
    foldMapPar(sentence, 0, sentence.length, wordCountMonoid)(wordCountFunction) match {
      case Stub(s) => unstub(s)
      case Part(l, words, r) => unstub(l) + words + unstub(r)
    }
  }

  def main(args: Array[String]): Unit = {
    val source = readFile("src/main/scala/WordCount/big.txt")
    println(s"Count sequential:   ${wordCountSeq(source)}")
    println(s"Count parallel:     ${wordCountPar(source)}")

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 10,
      Key.exec.maxWarmupRuns -> 50,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ).withWarmer(new Warmer.Default)
    val seqTime = standardConfig.measure(wordCountSeq(source))
    val parTime = standardConfig.measure(wordCountPar(source))

    println(s"sequential time:  $seqTime")
    println(s"parallel time:    $parTime")
    println(s"speedup           ${seqTime.value / parTime.value}")
  }

}