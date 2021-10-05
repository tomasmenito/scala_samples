class CreditCard {}

class Coffee(var price: Int = 1) {}

class Payments {
  def charge(cc: CreditCard, price: Int) = { println("charging" + price) }
}
class Cafe {
  def buyCoffee(cc: CreditCard, p: Payments): Coffee = {
    val cup = new Coffee()
    p.charge(cc, cup.price)
    cup
  }
}

new Cafe().buyCoffee(new CreditCard(), new Payments())
