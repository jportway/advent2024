object Day11 {

  inline def numDigits(n: Long): Int =
    Math.floor(Math.log10(n)).toInt + 1

  def changes(digit: Long): List[Long] = {
    if (digit == 0) {
      List(1)
    } else {
      val str = digit.toString
      val len = str.length
      if (len % 2 == 0) {
        val (a, b) = str.splitAt(len / 2)
        List(a.toLong, b.toLong)
      } else {
        List(digit * 2024L)
      }
    }
  }

  def cycle(in: List[Long]) = in.flatMap(changes)

  @main
  def Day11Main(): Unit = {
    val testIn = List(125L, 17L)
    val result = (0 until 25).foldLeft(testIn)((acc, _) => cycle(acc))
    println(result.length)

    val input   = List(7725L, 185L, 2L, 132869L, 0L, 1840437L, 62L, 26310L)
    val result2 = (0 until 25).foldLeft(input)((acc, _) => cycle(acc))
    println(result2.length)

  }

}
