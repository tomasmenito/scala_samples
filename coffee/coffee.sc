class CreditCard {}

class Coffee(var price: Int = 1) {}

class Payments {
  def charge(cc: CreditCard, price: Int) = { println("charging" + price) }
}

case class Charge(cc: CreditCard, price: Int) {
  def combine(other: Charge): Charge =
    if (cc == other.cc)
      Charge(cc, price + other.price)
    else
      throw new Exception("Can't combine charges from different cards")
}
class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))
  }
}

new Cafe().buyCoffee(new CreditCard())
