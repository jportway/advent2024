import scala.util.matching.Regex

object Day3 {
  @main
  def day3Main(): Unit = {
    println(
      doCalc(
        """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"""
      )
    )

    val result: (Int, Boolean) = doCalc(os.read(os.pwd / "input" / "day3.txt"))
    println(s"sum of all matches : $result")
  }

  def doCalc(input: String) = {
    val exp: Regex = """(mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\))""".r
    val matches = exp.findAllMatchIn(input)
    val result = matches.foldLeft(0, true) { case ((acc, active), m) =>
      m match {
        case exp("do()", _, _)    => (acc, true)
        case exp("don't()", _, _) => (acc, false)
        case exp(_, a, b) =>
          if (active) (acc + (a.toInt * b.toInt), active) else (acc, active)
      }
    }
    result
  }
}
