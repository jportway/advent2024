import scala.annotation.targetName

object Day4 {

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

    /** checks if the character at the given location matches the given character will always return false if the
      * location is invalid
      */
    def matchChar(pos: Location, toMatch: Char): Boolean = pos.content.contains(toMatch)

    /** all valid locations in the matrix */
    def locations: Seq[ValidLocation] = {
      for {
        i <- LazyList.range(0, contents.length)
        j <- LazyList.range(0, contents(i).length)
      } yield ValidLocation(j, i)
    }

    /** calls a function repeatedly with every location in the matrix, producing a one dimensional sequence of reuslts
      */
    def iterateOverLocations[T](f: (txt: this.type, loc: ValidLocation) => T): Seq[T] =
      locations.map(loc => f(this, loc))

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

  @main
  def test(): Unit = {

    // for each location in the matrix, check all directions for the given string and count how many instances you find
    def findWords(input: TextMatrix, searchString: String): Int = input.locations.foldLeft(0) { (acc, loc) =>
      val numFound = Direction.allDirections.count(dir => input.stringCheck(searchString, loc, dir))
      acc + numFound
    }

    val in = TextMatrix(os.read.lines(os.pwd / "input" / "day4.txt"))

    println(s"number of XMAS : ${findWords(in, "XMAS")}")

    val midpoints = in.locations.filter(_.value == 'A')
    val X_mas = midpoints.count { loc =>
      // check all 4 diagonals - a valid X_MAS will have 2 diagonals with MAS
      Direction.diagonals.count { dir =>
        in.stringCheck("MAS", loc + dir, -dir)
      } == 2
    }

    println(s"number of X_MAS : $X_mas")
  }

}
