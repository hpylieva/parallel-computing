package ua.edu.ucu.cs.parallel

import scala.util.Random

object Monoids{
  trait Monoid[A]{
    def op(x: A, y: A): A
    def zero: A
  }

  def stringMonoid: Monoid[String] = new Monoid[String] {
    def op(x: String, y: String) = x + y
    def zero  = ""
  }

  def listMonoid[A] = new Monoid[List[A]]{
    def op(x: List[A], y: List[A]) =  x++y
    def zero = Nil
  }

  def concat[A](xs: List[A], m: Monoid[A]): A =
    xs.foldLeft(m.zero)(m.op)

  def foldMap[A, B](xs: List[A], m:Monoid[B])(f:A=>B): B =
    xs.foldLeft(m.zero)((b,a) => m.op(b, f(a)))

  //  indexedSeq == enumerate in Python
  def foldMapBalanced [A, B] ( xs : IndexedSeq [A] , m: Monoid [B] )
                             (f: A => B): B =
    if (xs.isEmpty)
      m.zero
    else if (xs.length ==1)
      f(xs(0))
    else {
      val (l,r) = xs.splitAt(xs.length /2)
      m.op(foldMapBalanced(l, m)(f),
        foldMapBalanced(r,m)(f))
    }

  /**
    * Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You’ll
    * need to come up with a creative Monoid.
    */
  def seqIsOrdered(ints: IndexedSeq[Int]): Boolean = {
    // Reference implementation
    // (Int, Int, Boolean) = (Min, Max, IsOrdered)
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]):
      Option[(Int, Int, Boolean)] =
        (o1, o2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      val zero: None.type = None
    }
    foldMapBalanced(ints, mon)(i => Some((i, i, true))).forall(_._3)
  }

  // Parallel folding
  def foldPar[A](xs: IndexedSeq[A],
                       from: Int, to: Int, m: Monoid[A])
                      (implicit thresholdSize: Int): A =
    if (to-from < thresholdSize)
      foldSegment(xs, from, to, m)
    else {
      val middle = from + (to - from) / 2
      val (l, r) = parallel(
        foldPar(xs, from, middle, m)(thresholdSize),
        foldPar(xs, middle, to, m)(thresholdSize))
      m.op(l, r)
    }

  def foldSegment[A](xs: IndexedSeq[A],from: Int, to: Int,
                          m: Monoid[A]): A = {
    var res = xs(from)
    var index = from + 1
    while (index < to){
      res = m.op(res, xs(index))
      index = index + 1
    }
    res
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
      index = index + 1
    }
    res
  }

  def power(x: Int, p: Double): Int =
    math.exp(p * math.log(Math.abs(x))).toInt


  def main(args: Array[String]): Unit = {
    val rnd = new Random
    val length = 1000000

    // computing the sum of squares
    val source1 = (0 until length).map(_ * rnd.nextInt()).toVector

    val monoid1 = new Monoid[Int] {
      def op(x: Int, y: Int): Int = x + y
      def zero = 0
    }

    //  no relationship between the var in context and the implicit variable in the function declaration
    // during the function call the compiler will find the closest definition of an implicit in the scope
    //  which matches the type requirement
    implicit val threshold: Int = 1000
    val res1 = foldMapPar(source1, 0, source1.length, monoid1)(power(_,2))
    println(s"Sum of squares of a vector of len $length is $res1")


    // computing the sum of positive integers and their number in the sequence
    val source2 = (0 until length).
      map(n => {val x= rnd.nextInt(100)
      if (x%2 ==0) x else -x}).toVector

    // the first value will be the sum of positive elements
    // and the 2nd is the number of positive elements
    val monoid2 = new Monoid[(Int, Int)] {
      def op(x: (Int, Int), y: (Int, Int)): (Int, Int) = {
        val x_ = if (x._1 >= 0) x else zero
        val y_ = if (y._1 >= 0) y else zero
        (x_._1 + y_._1, x_._2 + y_._2)
      }
      def zero: (Int, Int) = (0,0)
    }

    val res2 = foldMapPar[Int, (Int, Int)](source2, 0, source2.length, monoid2)((_,1))
    println(s"Sum and mnumber of positive elements of a Vector of len $length is $res2")
  }
}
