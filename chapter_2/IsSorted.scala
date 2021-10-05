object IsSorted {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(index: Int): Boolean =
      if (index >= as.length) true
      else if (!ordered(as(index - 1), as(index))) false
      else loop(index + 1)

    loop(1)
  }
  def main(args: Array[String]): Unit = {
    val l: Array[Int] = Array(1, 2, 3)
    println(isSorted(l, ((a: Int, b: Int) => a < b)))
  }
}
