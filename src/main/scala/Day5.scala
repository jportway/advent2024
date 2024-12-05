object Day5 {

  @main
  def day5Main(): Unit = {
    val lines = os.read.lines(os.pwd / "input" / "day5.txt")
    val regex = """(\d+)\|(\d+)""".r
    val rules = lines.takeWhile(_.nonEmpty).map { case regex(a, b) =>
      (a.toInt, b.toInt)
    }
    val updates = lines.dropWhile(_.nonEmpty).drop(1).map(_.split(",").map(_.toInt).toVector)

    def validUpdate(update: IndexedSeq[Int]) = {
      val valid = !rules.exists { case (a, b) =>
        val ia = update.indexOf(a)
        val ib = update.indexOf(b)
        (ia != -1 && ib != -1) && (ia > ib)
      }
      valid
    }

    def middleNumber(arr: IndexedSeq[Int]): Int =
      arr(arr.length / 2)

    class TotalOrder extends Ordering[Int] {
      override def tryCompare(a: Int, b: Int): Some[Int] = {
        val rule = rules.find { case (x, y) => (x == a && y == b) || (x == b && y == a) }
        rule match {
          case Some((x, y)) => Some(if (a == x) -1 else 1)
          case None         => Some(a - b) // default to natural order
        }
      }
      override def compare(x: Int, y: Int): Int = tryCompare(x, y).get
    }

    val valid         = updates.filter(validUpdate)
    val middleNumbers = valid.map(middleNumber(_))
    val sum           = middleNumbers.sum
    println(s"sum of middle numbers $sum")

    val invalid   = updates.filterNot(validUpdate)
    val sorted    = invalid.map(_.sorted(new TotalOrder))
    val middleSum = sorted.map(middleNumber(_)).sum
    println(s"sum of middle numbers of sorted updates $middleSum")
  }

}
