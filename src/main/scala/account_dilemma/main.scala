object Main {
def startThread ( a : Account , b : Account , amount : Int ) = {
  val thread = new Thread {
    override def run = {
      (0 until amount) map { _ =>
        a.transfer(b, 1)
      }
    }
  }
  thread.start
  thread
}


  def main(args: Array[String]): Unit = {
    val a = new Account(12399, 100)
    val b = new Account(43578, 100)

    println(s"Accounts before transfer: \n\t$a\n\t$b\n")

    val t1 = startThread(a, b, 10)
    val t2 = startThread(b, a, 20)

    t1.join
    t2.join

    println(s"Accounts after transfer: \n\t$a\n\t$b\n")
  }
}