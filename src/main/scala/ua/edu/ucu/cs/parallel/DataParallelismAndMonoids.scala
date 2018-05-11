package ua.edu.ucu.cs.parallel

object DataParallelismAndMonoids{
  // (B,A) => B - wont concentrate on transforming one type to another

  def fold[A]
  (list: List[A])
  (zero: A)
  (op: (A,A) => A): A = {
    if(list.isEmpty) zero
    else op(list.head, fold(list.tail)(zero)(op))
  }

  // want to obtain intermediate results of fold
//  def scan[A](list: List[A])(zero: A)(op: (A,A) => A): List[A] = {
//    def scan0(input: List[A], resultList: List[A], resultVal: A): List[A] = {
//      if (input.isEmpty) resultList
//      else {
//        val newResultVal = op(input.head, resultVal)
//        scan0(input.tail, newResultVal :: resultList, newResultVal)
//      }
//    }
//    scan0(list, List(), zero)
//  }
    def scan[A](list: List[A])(zero: A)(op: (A,A) => A): List[A] = {
      def scan0(input: List[A], resultList: List[A]): List[A] = {
        if (input.isEmpty) resultList
        else scan0(input.tail, op(input.head, resultList.head) :: resultList)
      }
      scan0(list, List(zero))
    }

  val threshold = 1000

  def mapArraySegment[A, B](source: Array[A], from: Int, to: Int,
                            f: A => B, target: Array[B]): Unit = {
    var index = from
    while (index < to) {
      target(index) = f(source(index))
      index = index + 1
    }
  }

  def mapArraySegmentPar[A, B](source: Array[A], from: Int, to: Int,
                            f: A => B, target: Array[B]): Unit ={
    if (to - from < threshold) mapArraySegment(source, from, to, f, target)
    else{
      val middle = from + (to - from)/2
      val (l,r) = parallel(mapArraySegmentPar(source, from, middle, f, target),
        mapArraySegmentPar(source, middle, to, f, target))
    }
  }

  // now we want to represent array as a tree
  sealed trait Tree[T]
  case class Leaf[T](block: Array[T]) extends Tree[T]
  case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]

  // some error
//  def mapTreePar[A, B](source: Tree[A], from: Int, to: Int,
//                               f: A => B): Tree[B] = source match{
//    case Leaf(block) => {
//      val newBlock = new Array[B](block.length)
//      mapArraySegment(block, 0, block.length, f, newBlock)
//      Leaf(newBlock)
//    }
//    case _ => throw new IllegalArgumentException
//  }

  def main(args: Array[String]): Unit = {
    println(fold(List(1,2,3))(42)( (x, acc) => x + acc))
    println(scan(List(1,2,3))(42)( (x, acc) => x + acc))
  }
}