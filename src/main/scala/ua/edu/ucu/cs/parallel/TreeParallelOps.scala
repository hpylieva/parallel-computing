package ua.edu.ucu.cs.parallel

object TreeParallelOps{
//  as JVM doesn't perform varification of parameters before Run time,
// to instantialte the arrays with size field we use Manifest
  def mapTreePar[A: Manifest, B:Manifest](
                 tree: Tree[A],
                 f: A => B): Tree[B] =
  tree match {
    case Leaf(block) => {
      val newBlock = new Array[B](block.length)
      var index = 0
      while (index<block.length){
        newBlock(index) = f(block(index))
        index = index +1
      }
      Leaf(newBlock)
    }
    case Node(left, right) => {
      val(l, r) = parallel(mapTreePar(left, f),
                           mapTreePar(right, f))
      Node(l, r)
    }
  }
}
