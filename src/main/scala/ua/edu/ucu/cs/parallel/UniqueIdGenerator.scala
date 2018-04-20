object UniqueIdGenerator {
  val x = new AnyRef{}
  var uidCount = 0L

  def genUniqueId: Long = {
    uidCount = uidCount + 1
    uidCount
  }

  def startThread = {
    val thread = new Thread {
      override def run = {
        val uids = for (i <- 0 until 10) yield genUniqueId
        println(uids)
      }
    }
    thread.start
    thread
  }

  def syncGenUniqueId: Long = {
    x.synchronized {
      uidCount = uidCount + 1
      uidCount
    }
  }

  def syncStartThread = {
    val thread = new Thread {
      override def run = {
        val uids = for (i <- 0 until 10) yield syncGenUniqueId
        println(uids)
      }
    }
    thread.start
    thread
  }
  def main(args: Array[String]): Unit = {
    // startThread
    // startThread
    // without x - monitor:
    // Vector(1, 2, 3, 3, 5, 7, 9, 11, 13, 15)
    // Vector(1, 2, 4, 6, 8, 10, 12, 14, 16, 17)
    // so the values in vectors repeat
    syncStartThread
    syncStartThread
    // now 2 vectors contain the values which doesn't repeat due to synchronization
  }

}