class CreditCard {
  def charge(price: Int) = { println("charging" + price) }
}

class Coffee(var price: Int = 1) {}

class Cafe {
  def buyCoffee(cc: CreditCard): Coffee = {
    val cup = new Coffee()
    cc.charge(cup.price)
    cup
  }
}

new Cafe().buyCoffee(new CreditCard())
