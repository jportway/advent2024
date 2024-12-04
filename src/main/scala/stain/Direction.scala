package stain

import scala.annotation.targetName

/** represents a direction within a TextMatrix */
case class Direction(x: Int, y: Int) {

  @targetName("addDirection")
  def +(other: Direction): Direction = Direction(x + other.x, y + other.y)

  def opposite: Direction = Direction(-x, -y)
  def unary_- : Direction = opposite

}

object Direction {

  val up                            = Direction(0, -1)
  val down                          = Direction(0, 1)
  val left                          = Direction(-1, 0)
  val right                         = Direction(1, 0)
  val upLeft                        = up + left
  val upRight                       = up + right
  val downLeft                      = down + left
  val downRight                     = down + right
  val cardinals: Seq[Direction]     = Vector(up, down, left, right)
  val diagonals: Seq[Direction]     = Vector(upLeft, upRight, downLeft, downRight)
  val allDirections: Seq[Direction] = cardinals ++ diagonals

}
