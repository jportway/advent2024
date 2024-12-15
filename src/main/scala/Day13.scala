import stain.AbstractPosition
import stain.Direction
import stain.Position

import scala.annotation.tailrec
import scala.annotation.targetName
import scala.util.matching.Regex

object Day13 {

  case class LongVec(x: Long, y: Long) {

    @targetName("mult")
    def *(scalar: Long): LongVec = LongVec(x * scalar, y * scalar)

    @targetName("sub")
    def -(other: LongVec): LongVec = LongVec(x - other.x, y - other.y)

    @targetName("scalarDiv")
    def /(scalar: Long): LongVec = LongVec(x / scalar, y / scalar)

    @targetName("div")
    def /(other: LongVec): Long = {
      val xDiv = x / other.x
      val yDiv = y / other.y
      Math.min(xDiv, yDiv)
    }

    def modZero(other: LongVec): Boolean =
      (x % other.x == 0) & (y % other.y == 0)

  }

  case class Problem(buttonA: LongVec, buttonB: LongVec, target: LongVec)

  @main
  def day13Main(): Unit = {
    val file  = os.read(os.pwd / "input" / "day13.txt")
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

    def findSolution(problem: Problem): Option[(Long, Long)] = {
      import problem.*
      val det = buttonA.x * buttonB.y - buttonA.y * buttonB.x
      if (det == 0) {                          // vectors are colinear
        Option.when(target.modZero(buttonA)) { // None if we can't reach the target
          val a = target / buttonA
          val b = target / buttonB
          if (a * 3 < b) (a, 0) else (0, b)
        }
      } else {
        val xTop   = target.x * buttonB.y - target.y * buttonB.x
        val xValid = xTop % det == 0 // if not 0 then doesn't fit exactly
        val x      = xTop / det
        val yTop   = target.y * buttonA.x - target.x * buttonA.y
        val yValid = yTop % det == 0 // if not 0 then doesn't fit exactly
        val y      = yTop / det
        Option.when(xValid & yValid)((x, y))
      }
    }

    val solutions = problems.map(findSolution)
    val cost      = solutions.collect { case Some(x) => x._1 * 3 + x._2 }.sum
    println(cost)

    val partBProblems =
      problems.map(p => p.copy(target = LongVec(p.target.x + 10000000000000L, p.target.y + 10000000000000L)))
    val solutionsB = partBProblems.map(findSolution)
    val costB      = solutionsB.collect { case Some(x) => x._1 * 3 + x._2 }.sum
    println(costB)
  }

}
