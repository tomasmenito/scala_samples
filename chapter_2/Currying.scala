object Currying {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => (b => f(a, b))

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def main(args: Array[String]): Unit = {
    println(curry((a: Int, b: Int) => a + b)(1)(2))
    println(uncurry((a: Int) => ((b: Int) => a + b))(1, 2))
  }

}
