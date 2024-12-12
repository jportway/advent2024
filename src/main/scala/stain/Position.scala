package stain

import scala.annotation.targetName

trait Position {

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

}

/** a location that only indicates a position and isn't tied to a particular TextMatrix */
case class AbstractPosition(x: Int, y: Int) extends Position {

  @targetName("addDirection")
  override def +(direction: Direction): AbstractPosition = AbstractPosition(x + direction.x, y + direction.y)

  @targetName("subDirection")
  override def -(direction: Direction): AbstractPosition = AbstractPosition(x - direction.x, y - direction.y)

  override def cardinalNeighbours: Vector[Position] = Direction.cardinals.map(this + _)

  override def diagonalNeighbours: Vector[Position] = Direction.diagonals.map(this + _)

  override def allNeighbours: Vector[Position] = cardinalNeighbours ++ diagonalNeighbours

}
