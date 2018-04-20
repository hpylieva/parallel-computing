package microbenchmarking
import org.scalameter
import org.scalameter._

object PerformanceMeasuring{
  def main(args: Array[String]): Unit = {
//    val time = measure {
//      (0 until 10000000).toArray
//    }
//    will give result in seconds which is smaller and smaller
//    not to observe this effect a Warm-ip is needed

   // with Warmer the results are much more consistent
    val time = withWarmer(new scalameter.Warmer.Default) measure {
        (0 until 1000000).toArray
      }
    println(time)
  }
}