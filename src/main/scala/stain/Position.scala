package stain

import scala.annotation.targetName

type Position = BasePosition[?]

trait BasePosition[C <: BasePosition[C]] {

  def x: Int

  def y: Int

  def left: Position = this + Direction.left

  def right: Position = this + Direction.right

  def up: Position = this + Direction.up

  def down: Position = this + Direction.down

  @targetName("addDirection")
  def +(direction: Direction): Position

  @targetName("subDirection")
  def -(direction: Direction): Position

  def vectorTo(other: Position): Direction = Direction(x - other.x, y - other.y)

  def cardinalNeighbours: Vector[Position]

  def diagonalNeighbours: Vector[Position]

  def allNeighbours: Vector[Position]

  def copy(x: Int, y: Int): C

  def wrap(width: Int, height: Int): C = {
    val x0 = ((x % width) + width)   % width
    val y0 = ((y % height) + height) % height
    copy(x0, y0)
  }

}

/** a location that only indicates a position and isn't tied to a particular TextMatrix */
case class SimplePos(x: Int, y: Int) extends BasePosition[SimplePos] {

  @targetName("addDirection")
  override def +(direction: Direction): SimplePos = SimplePos(x + direction.x, y + direction.y)

  @targetName("subDirection")
  override def -(direction: Direction): SimplePos = SimplePos(x - direction.x, y - direction.y)

  override def cardinalNeighbours: Vector[SimplePos] = Direction.cardinals.map(this + _)

  override def diagonalNeighbours: Vector[SimplePos] = Direction.diagonals.map(this + _)

  override def allNeighbours: Vector[SimplePos] = cardinalNeighbours ++ diagonalNeighbours

//  override def copy(x: Int, y: Int): AbstractPosition = AbstractPosition(x, y)

}
