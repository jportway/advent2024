import stain.AbstractPosition
import stain.Direction
import stain.Position

import scala.annotation.targetName
import scala.util.matching.Regex

object Day13 {

  case class LongVec(stepX:Long, stepY:Long) {
    @targetName("mult")
    def *(scalar:Long): LongVec = LongVec(stepX*scalar,stepY*scalar)

    @targetName("sub")
    def -(other:LongVec): LongVec = LongVec(stepX-other.stepX,stepY-other.stepY)

    @targetName("scalarDiv")
    def /(scalar:Long): LongVec = LongVec(stepX/scalar,stepY/scalar)

    @targetName("div")
    def /(other:LongVec):Long = {
      val xDiv = stepX/other.stepX
      val yDiv = stepY/other.stepY
      Math.min(xDiv,yDiv)
    }

    def modZero(other:LongVec):Boolean = {
      (stepX % other.stepX == 0) & (stepY % other.stepY == 0)
    }
  }

  case class Problem(buttonA: LongVec, buttonB: LongVec, target: LongVec)

  @main
  def day13Main(): Unit = {
    val file  = os.read(os.pwd / "input" / "day13test.txt")
    val regex = "Button A\\: X\\+(\\d+), Y\\+(\\d+)\\nButton B\\: X\\+(\\d+), Y\\+(\\d+)\\nPrize: X=(\\d+), Y=(\\d+)".r
    val problems = regex
      .findAllMatchIn(file)
      .map { m =>
        val buttonA = LongVec(m.group(1).toLong, m.group(2).toLong)
        val buttonB = LongVec(m.group(3).toLong, m.group(4).toLong)
        val target  = LongVec(m.group(5).toInt, m.group(6).toInt)
        Problem(buttonA, buttonB, target)
      }
      .toList

    // (x * A)+( y * B) = T
    // (y*B) = T-(x*A)
    // y = (T-(x*A))/B
    def solve(problem:Problem): Seq[(Long, Long)] = {
      val target = problem.target
      val a = problem.buttonA
      val b = problem.buttonB
      val xStep = (target / a)
      for{
        x <- 0L to xStep
        remaining = target-(a * x)
        if remaining.modZero(b)
        y = remaining / b
      } yield (x,y)
    }

    problems.foreach{ problem =>
      println(solve(problem))
    }

//    val partBProblems = problems.map(p => p.copy(target = Position(p.target.x + 10000000000000, p.target.y + 10000000000000)))

  }

}
