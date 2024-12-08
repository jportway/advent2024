package stain

import scala.annotation.targetName

trait Location {

  def x: Int

  def y: Int

  def content: Option[Char] = None

  def left: Location = this + Direction.left

  def right: Location = this + Direction.right

  def up: Location = this + Direction.up

  def down: Location = this + Direction.down

  @targetName("addDirection")
  def +(direction: Direction): Location = AbstractLocation(x + direction.x, y + direction.y)

  @targetName("subDirection")
  def -(direction: Direction): Location = AbstractLocation(x - direction.x, y - direction.y)

  def vectorTo(other: Location): Direction = Direction(x - other.x, y - other.y)

}

case class AbstractLocation(x: Int, y: Int) extends Location

case class TextMatrix(contents: IndexedSeq[String]) {

  abstract class ConcreteLocation extends Location {

    def isValid: Boolean

    def toOption: Option[ValidLocation]

    @targetName("addDirection")
    override def +(direction: Direction): ConcreteLocation = TextMatrix.this(x + direction.x, y + direction.y)

    @targetName("subDirection")
    override def -(direction: Direction): ConcreteLocation = TextMatrix.this(x - direction.x, y - direction.y)

  }

  case class InvalidLocation private[TextMatrix] (x: Int, y: Int) extends ConcreteLocation {

    override def isValid = false

    override def toOption: Option[ValidLocation] = None

    override def content: Option[Char] = None

  }

  case class ValidLocation private[TextMatrix] (x: Int, y: Int) extends ConcreteLocation {

    override def isValid = true

    override def toOption: Option[ValidLocation] = Some(this)

    override def content: Option[Char] = Some(contents(y)(x))

    def value: Char = contents(y)(x)

    def set(value: Char): TextMatrix = TextMatrix(contents.updated(y, contents(y).updated(x, value)))

  }

  def apply(x: Int, y: Int): ConcreteLocation = if (valid(x, y)) ValidLocation(x, y) else InvalidLocation(x, y)
  def apply(pos: Location): ConcreteLocation = pos match {
    case c: ConcreteLocation => c // it's already attached to this matrix
    case l: Location         => this(l.x, l.y)
  }
  def valid(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && y < contents.length && x < contents(y).length

  /** all valid locations in the matrix */
  def locations: Seq[ValidLocation] = {
    for {
      i <- LazyList.range(0, contents.length)
      j <- LazyList.range(0, contents(i).length)
    } yield ValidLocation(j, i)
  }

  /** checks if the character at the given location matches the given character will always return false if the location
    * is invalid
    */
  def matchChar(pos: Location, toMatch: Char): Boolean = pos.content.contains(toMatch)

  /** check for a string starting from a particular location in a particular direction */
  def stringCheck(searchString: String, currentPos: Location, direction: Direction): Boolean = {
    if searchString.isEmpty then true
    else {
      val matched = matchChar(currentPos, searchString.head)
      if matched then stringCheck(searchString.tail, currentPos + direction, direction)
      else false
    }
  }

  def find(test: (Option[Char]) => Boolean): Seq[ValidLocation] = locations.filter(loc => test(loc.content))

}
