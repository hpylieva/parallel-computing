package ua.edu.ucu.cs.parallel

sealed trait Tree[T]{
  val size: Int
}

case class Leaf[T](block: Array[T]) extends  Tree[T]{
  override val size: Int = block.size
}

case class Node[T](left: Tree[T], right: Tree[T])
                                  extends Tree[T]{
  override val size: Int = left.size + right.size
}
