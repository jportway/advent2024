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

}

/** a location that only indicates a position and isn't tied to a particular TextMatrix */
case class AbstractPosition(x: Int, y: Int) extends Position {

  @targetName("addDirection")
  override def +(direction: Direction): AbstractPosition = AbstractPosition(x + direction.x, y + direction.y)

  @targetName("subDirection")
  override def -(direction: Direction): AbstractPosition = AbstractPosition(x - direction.x, y - direction.y)

}

case class TextMatrix(contents: IndexedSeq[String]) {

  /** either an InvalidLocation or ValidLocation */
  abstract class Cell extends Position {

    def isValid: Boolean

    def toOption: Option[ValidCell]

    def content: Option[Char] // the contents of the cell, if it's valid

    @targetName("addDirection")
    override def +(direction: Direction): Cell = TextMatrix.this(x + direction.x, y + direction.y)

    @targetName("subDirection")
    override def -(direction: Direction): Cell = TextMatrix.this(x - direction.x, y - direction.y)

  }

  /** a location that isn't within the bounds of the matrix */
  case class InvalidCell private[TextMatrix] (x: Int, y: Int) extends Cell {

    override def isValid = false

    override def toOption: Option[ValidCell] = None

    override def content: Option[Char] = None

  }

  /** a location that is within the bounds of the matrix */
  case class ValidCell private[TextMatrix] (x: Int, y: Int) extends Cell {

    override def isValid = true

    override def toOption: Option[ValidCell] = Some(this)

    override def content: Option[Char] = Some(contents(y)(x))

    def value: Char = contents(y)(x)

    def set(value: Char): TextMatrix = TextMatrix(contents.updated(y, contents(y).updated(x, value)))

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

  def contents(x: Int, y: Int): Char = contents(y)(x)

  def update(x: Int, y: Int, value: Char): TextMatrix = TextMatrix(contents.updated(y, contents(y).updated(x, value)))
  def update(pos: Position, value: Char): TextMatrix  = update(pos.x, pos.y, value)
  def update(values: List[(Position, Char)]): TextMatrix = values.foldLeft(this) { case (acc, (pos, value)) =>
    acc.update(pos, value)
  }

  /** checks if the character at the given location matches the given character will always return false if the location
    * is invalid
    */
  def matchChar(pos: Position, toMatch: Char): Boolean = valid(pos) && (contents(pos.x, pos.y) == toMatch)

  /** check for a string starting from a particular location in a particular direction */
  def stringCheck(searchString: String, currentPos: Position, direction: Direction): Boolean = {
    if searchString.isEmpty then true
    else {
      val matched = matchChar(currentPos, searchString.head)
      if matched then stringCheck(searchString.tail, currentPos + direction, direction)
      else false
    }
  }

  def find(test: (Option[Char]) => Boolean): Seq[ValidCell] = locations.filter(loc => test(loc.content))

}
