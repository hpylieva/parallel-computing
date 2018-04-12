object mainBenchmarking{
  def main(args: Array[String]): Unit = {
    val xs = List( 1 , 2 , 3 , 42)
    val startTime = System.nanoTime
    xs.reverse
    val time = ( System . nanoTime - startTime) / 1000000
    println( s"Execution time $time  millis." )
  }
}