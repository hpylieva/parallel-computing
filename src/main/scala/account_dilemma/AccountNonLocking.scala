package account_dilemma

class AccountNonLocking(number : Long, initialAmount : Int ) {
  private val uid = number
  private var amount = initialAmount

  private def lockAndTransfer(to: AccountNonLocking, amount: Int) =
    this.synchronized {
      to.synchronized {
        this.amount -= amount
        to.amount += amount
      }
    }

  def transfer(to: AccountNonLocking, amount: Int) =
    if (this.uid < to.uid) this.lockAndTransfer(to, amount)
    else to.lockAndTransfer(this, -amount)

  override def toString = s"Account number = $uid amount = $amount"
}

