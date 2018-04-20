import account_dilemma.AccountNonLocking

object Main {
def startThread (a : AccountNonLocking, b : AccountNonLocking, amount : Int ) = {
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
    val a = new AccountNonLocking(12399, 1000)
    val b = new AccountNonLocking(43578, 1000)

    println(s"Accounts before transfer: \n\t$a\n\t$b\n")
    // this will crate a dead lock
    val t1 = startThread(a, b, 10)
    val t2 = startThread(b, a, 20)

    t1.join
    t2.join

    println(s"Accounts after transfer: \n\t$a\n\t$b\n")
  }
}