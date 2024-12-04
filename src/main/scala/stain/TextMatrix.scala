package stain

import scala.annotation.targetName

case class TextMatrix(contents: IndexedSeq[String]) {

  trait Location {

    def x: Int

    def y: Int

    def isValid: Boolean

    def toOption: Option[ValidLocation]

    def content: Option[Char]

    def left: Location = this + Direction.left

    def right: Location = this + Direction.right

    def up: Location = this + Direction.up

    def down: Location = this + Direction.down

    @targetName("addDirection")
    def +(direction: Direction): Location = Location(x + direction.x, y + direction.y)

  }

  object Location {

    def apply(x: Int, y: Int): Location = if (valid(x, y)) ValidLocation(x, y) else InvalidLocation(x, y)

    def valid(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && y < contents.length && x < contents(y).length

  }

  case class InvalidLocation private[TextMatrix] (x: Int, y: Int) extends Location {

    def isValid = false

    def toOption: Option[Nothing] = None

    def content: Option[Nothing] = None

  }

  case class ValidLocation private[TextMatrix] (x: Int, y: Int) extends Location {

    def isValid = true

    def toOption: Option[ValidLocation] = Some(this)

    def content: Option[Char] = Some(contents(y)(x))

    def value: Char = contents(y)(x)

  }

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

}
