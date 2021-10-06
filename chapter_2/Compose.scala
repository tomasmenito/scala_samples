object Compose {
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]): Unit = {
    val sumOne = (a: Int) => a + 1
    val multiplyByTwo = (b: Int) => b * 2
    println(compose(sumOne, multiplyByTwo)(5))
  }
}
