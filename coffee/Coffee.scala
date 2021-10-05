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

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }
}

new Cafe().buyCoffee(new CreditCard())
