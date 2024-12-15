package stain

import scala.annotation.targetName

/** represents a direction within a TextMatrix */
case class Direction(x: Int, y: Int) {

  @targetName("addDirection")
  def +(other: Direction): Direction = Direction(x + other.x, y + other.y)
  @targetName("multDirection")
  def *(scalar: Int): Direction = Direction(x * scalar, y * scalar)
  def opposite: Direction       = Direction(-x, -y)
  def unary_- : Direction       = opposite

  def turnRight: Direction = Direction(-y, x)
  def turnLeft: Direction  = Direction(y, -x)

}

object Direction {

  val up                               = Direction(0, -1)
  val down                             = Direction(0, 1)
  val left                             = Direction(-1, 0)
  val right                            = Direction(1, 0)
  val upLeft                           = up + left
  val upRight                          = up + right
  val downLeft                         = down + left
  val downRight                        = down + right
  val cardinals: Vector[Direction]     = Vector(up, down, left, right)
  val diagonals: Vector[Direction]     = Vector(upLeft, upRight, downLeft, downRight)
  val allDirections: Vector[Direction] = cardinals ++ diagonals

}
