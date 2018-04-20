class AccountLocking(number : Long, initialAmount : Int ) {
  private val uid = number
  private var amount = initialAmount

  def transfer(to: AccountLocking, amount: Int) =
  // not global monitors as this will block all the accounts when transaction between some 2 is held
    this.synchronized {
      to.synchronized {
        this.amount -= amount
        to.amount += amount
      }
    }

  override def toString = s"Account number = $uid amount = $amount"
}

