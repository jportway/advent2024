package stain

import scala.annotation.targetName

abstract class SpacialMatrix[C <: SpacialMatrix[C, T], T] {

  def contents: IndexedSeq[IndexedSeq[T]]
  // subclasses implement this to create a copy of themselves with the given contents
  protected def copyWithContents(contents: IndexedSeq[IndexedSeq[T]]): C

  /** either an InvalidLocation or ValidLocation */
  abstract class Cell extends Position {

    def isValid: Boolean

    def toOption: Option[ValidCell]

    def content: Option[T] // the contents of the cell, if it's valid

    def contains(test: T) = content.contains(test)

    @targetName("addDirection")
    override def +(direction: Direction): Cell = SpacialMatrix.this(x + direction.x, y + direction.y)

    @targetName("subDirection")
    override def -(direction: Direction): Cell = SpacialMatrix.this(x - direction.x, y - direction.y)

    override def cardinalNeighbours: Vector[Cell] = Direction.cardinals.map(this + _)

    override def diagonalNeighbours: Vector[Cell] = Direction.diagonals.map(this + _)

    override def allNeighbours: Vector[Cell] = cardinalNeighbours ++ diagonalNeighbours

  }

  /** a location that isn't within the bounds of the matrix */
  case class InvalidCell private[SpacialMatrix] (x: Int, y: Int) extends Cell {

    override def isValid = false

    override def toOption: Option[ValidCell] = None

    override def content: Option[T] = None

  }

  /** a location that is within the bounds of the matrix */
  case class ValidCell private[SpacialMatrix] (x: Int, y: Int) extends Cell {

    override def isValid = true

    override def toOption: Option[ValidCell] = Some(this)

    override def content: Option[T] = Some(contents(y)(x))

    def value: T = contents(y)(x)

    def set(value: T): C = copyWithContents(contents.updated(y, contents(y).updated(x, value)))

  }

  def apply(x: Int, y: Int): Cell = if (valid(x, y)) ValidCell(x, y) else InvalidCell(x, y)

  def apply(pos: Position): Cell = pos match {
    case c: Cell     => c // it's already attached to this matrix
    case l: Position => this(l.x, l.y)
  }

  def valid(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && y < contents.length && x < contents(y).length
  def valid(p: Position): Boolean    = valid(p.x, p.y)

  /** all valid locations in the matrix */
  def locations: Seq[ValidCell] = {
    for {
      i <- LazyList.range(0, contents.length)
      j <- LazyList.range(0, contents(i).length)
    } yield ValidCell(j, i)
  }

  def contents(x: Int, y: Int): T = contents(y)(x)

  def update(x: Int, y: Int, value: T): C = copyWithContents(contents.updated(y, contents(y).updated(x, value)))
  def update(pos: Position, value: T): C  = update(pos.x, pos.y, value)
  def update(values: List[(Position, T)]): C = values.foldLeft(this.asInstanceOf[C]) { case (acc: C, (pos, value)) =>
    acc.update(pos, value)
  }

  /** checks if the character at the given location matches the given character will always return false if the location
    * is invalid
    */
  def matchVal(pos: Position, toMatch: T): Boolean = valid(pos) && (contents(pos.x, pos.y) == toMatch)

  def find(test: T => Boolean): Seq[ValidCell] = locations.filter(loc => test(loc.value))

}