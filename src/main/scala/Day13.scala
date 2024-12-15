import stain.AbstractPosition
import stain.Direction
import stain.Position

import scala.util.matching.Regex

object Day13 {

  case class Problem(buttonA: Direction, buttonB: Direction, target: AbstractPosition)

  @main
  def day13Main(): Unit = {
    val file  = os.read(os.pwd / "input" / "day13.txt")
    val regex = "Button A\\: X\\+(\\d+), Y\\+(\\d+)\\nButton B\\: X\\+(\\d+), Y\\+(\\d+)\\nPrize: X=(\\d+), Y=(\\d+)".r
    val problems = regex
      .findAllMatchIn(file)
      .map { m =>
        val buttonA = Direction(m.group(1).toInt, m.group(2).toInt)
        val buttonB = Direction(m.group(3).toInt, m.group(4).toInt)
        val target  = AbstractPosition(m.group(5).toInt, m.group(6).toInt)
        Problem(buttonA, buttonB, target)
      }
      .toList

    // why not just brute force it ?
    val bestSolutions = problems.map { problem =>
      val solutions = for {
        aPress <- 0 to 100
        bPress <- 0 to 100
        total   = AbstractPosition(0, 0) + ((problem.buttonA * aPress) + (problem.buttonB * bPress))
        if total == problem.target
        cost = aPress * 3 + bPress
      } yield (aPress, bPress, cost)
      if solutions.isEmpty then None else Some(solutions.minBy((_, _, cost) => cost))
    }
    println(bestSolutions)
    val onlyValid = bestSolutions.collect { case Some((aPress, bPress, cost)) => cost }
    val totalCost = onlyValid.sum
    println(totalCost)
  }

}
