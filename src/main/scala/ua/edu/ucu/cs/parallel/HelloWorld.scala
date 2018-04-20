object HelloWorld {

  class GdayThread extends Thread {
    override def run() {
      // in this case the next option is possible
      // G`day G`day world! word!
      // because the operation is not atomic - we are printing twice
      println("G`day")
      println("world!")
    }
  }

  def main(args: Array[String]): Unit = {
    val thread1 = new GdayThread
    val thread2 = new GdayThread

    thread1.start()
    thread2.start()
    thread1.join()
    thread2.join()
  }

}