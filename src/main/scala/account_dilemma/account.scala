class Account ( number : Long , initialAmount : Int ) {
  private val uid = number
  private var amount = initialAmount

  def transfer(to: Account, amount: Int) =
    this.synchronized {
      to.synchronized {
        this.amount -= amount
        to.amount += amount
      }
    }

  override def toString = s"Account number = $uid amount = $amount"
}

