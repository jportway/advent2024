package stain

import scala.annotation.targetName

abstract class SpacialMatrix[C <: SpacialMatrix[C, T], T] {

  type Region = Seq[ValidCell]

  def contents: IndexedSeq[IndexedSeq[T]]
  val numColumns = contents.head.length // maybe should assert all rows same length?
  val numRows    = contents.length

  // subclasses implement this to create a copy of themselves with the given contents
  protected def copyWithContents(newContents: IndexedSeq[IndexedSeq[T]]): C

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
      i <- LazyList.range(0, numRows)
      j <- LazyList.range(0, numColumns)
    } yield ValidCell(j, i)
  }

  def rowContents: Seq[IndexedSeq[T]] = contents.toSeq
  def rows: Seq[Seq[ValidCell]] = for {
    i <- LazyList.range(0, numRows)
    r  = LazyList.range(0, numColumns).map(c => ValidCell(c, i))
  } yield r

  def columns: Seq[Seq[ValidCell]] = for {
    i <- LazyList.range(0, numColumns)
    c  = LazyList.range(0, numRows).map(r => ValidCell(i, r))
  } yield c

  def contents(x: Int, y: Int): T = contents(y)(x)
  def contents(pos: Position): T  = contents(pos.x, pos.y)

  /** all distinct values stored in the matrix */
  def distinct: Seq[T] = locations.map(_.value).distinct

  def update(x: Int, y: Int, value: T): C = copyWithContents(contents.updated(y, contents(y).updated(x, value)))
  def update(pos: Position, value: T): C  = update(pos.x, pos.y, value)
  def update(values: List[(Position, T)]): C = values.foldLeft(this.asInstanceOf[C]) { case (acc: C, (pos, value)) =>
    acc.update(pos, value)
  }

  /** a bit like a scanLeft - this iterates through the matrix.
    *
    * For each cell it calls the tranform function passing in the current cell, the previous cell and the previous row
    * @param transform
    *   : a function to transform each cell. It is passed the value of the current Cell, the value of the previous cell
    *   (if it exists) and the value of the corresponding cell on the row above (if it exists)
    */
  def scanRowsLeft[B](transform: (ValidCell, Option[B], Option[B]) => B): Seq[Seq[B]] = {
    val rows     = this.rows
    val firstRow = rows.head
    val firstRowTransformed = firstRow.tail.scanLeft(transform(firstRow.head, None, None)) {
      case (previousCell, cell) =>
        val transformed = transform(cell, Some(previousCell), None)
        transformed
    }
    rows.tail.scanLeft(firstRowTransformed) { (previousRow, row) =>
      val firstCell             = transform(row.head, None, Some(previousRow.head))
      val zippedWithPreviousRow = row.tail.zip(previousRow.tail)
      zippedWithPreviousRow
        .scanLeft(firstCell) { case (previousCell, (cell, previousRowCellValue)) =>
          val transformed = transform(cell, Some(previousCell), Some(previousRowCellValue))
          transformed
        }
        .toList
    }
  }

  /** create a new matrix by scanning each cell in this matrix. As in ScanRowsLeft the transform operation is passed the
    * current cell, the previous cell and the cell above in the previous row
    */
  def scanMap[B](transform: (ValidCell, Option[B], Option[B]) => B): SimpleMatrix[B] = {
    val transformedContents: Seq[Seq[B]] = scanRowsLeft(transform)
    val toVectors                        = transformedContents.map(_.toVector).toVector
    SimpleMatrix(toVectors)
  }

  def map[B](transform: (T) => B): SimpleMatrix[B] = {
    val mapped = contents.map(_.map(transform))
    SimpleMatrix(mapped)
  }

  /** checks if the character at the given location matches the given character will always return false if the location
    * is invalid
    */
  def matchVal(pos: Position, toMatch: T): Boolean = valid(pos) && (contents(pos.x, pos.y) == toMatch)

  def find(test: T => Boolean): Seq[ValidCell] = locations.filter(loc => test(loc.value))

  /** finds all cells connected to the start cell via the given directions that satisfy a test. This is a very simple
    * and unoptimised flood fill, so should probably be sorted out if scanning big matrices. Also it hides a secret
    * shame, which we will not dwell upon...
    * @param test
    *   function from CurrentCell, PreviousCell, direction from previous cell to current cell
    */
  def connected(start: ValidCell, directions: Seq[Direction])(
      test: (ValidCell, ValidCell, Direction) => Boolean
  ): Region = {
    val visited = Array.fill(numRows, numColumns)(false)
    visited(start.y)(start.x) = true
    List.unfold(Seq(start)) { toSearch =>
      if toSearch.isEmpty then None
      else {
        val cell = toSearch.head
        val foundCells: Seq[ValidCell] = directions.map { dir =>
          (cell + dir).toOption.flatMap { newCell =>
            if (!visited(newCell.y)(newCell.x)) {
              visited(newCell.y)(newCell.x) = true // i'm a naughty boy and should be punished
              Option.when(test(newCell, cell, dir))(newCell)
            } else {
              None
            }
          }
        }.collect { case Some(x) => x }
        val leftToSearch = toSearch.tail ++ foundCells
        Some(cell, leftToSearch)
      }
    }
  }

  /** divides the matrix into regions of connectivity.
    *
    * Be aware this scans from top left to bottom right and will greedily consume regions, so if your test is abiguous
    * the results will favour the ordering of the scan eg. if your test is something like :
    *
    * (cell,previousCell, Direction)=> cell.value == previousCell.value+1
    *
    * then it is possible a particular cell could belong to several different regions. In this case the first region
    * tested in the scan will win. Basically this is just implemented in a naive way by iteratively scanning every cell
    * and searching for a region connected to it
    */
  def connectedRegions(directions: Seq[Direction])(
      test: (ValidCell, ValidCell, Direction) => Boolean
  ): Seq[Region] = {
    val allocated = Array.fill(numRows, numColumns)(false)
    locations.foldLeft(List.empty) { (acc, cell) =>
      if allocated(cell.y)(cell.x) then acc
      else {
        val region = connected(cell, directions)(test)
        region.foreach(cell => allocated(cell.y)(cell.x) = true) // oops, I did it again
        region :: acc
      }
    }
  }

  def viz(view: (T => Char)): String = {
    val lines = for {
      row <- rowContents
      line = row.map(c => view(c)).mkString
    } yield line
    lines.mkString("\n")
  }

}

case class SimpleMatrix[T](contents: IndexedSeq[IndexedSeq[T]]) extends SpacialMatrix[SimpleMatrix[T], T] {

  override protected def copyWithContents(newContents: IndexedSeq[IndexedSeq[T]]): SimpleMatrix[T] =
    copy(contents = newContents)

}

object SimpleMatrix {

  def fill[T](numRows: Int, numColumns: Int)(func: (x: Int, y: Int) => T): SimpleMatrix[T] = {
    val content = (0 until numRows).map(i => (0 until numColumns).map(j => func(i, j)).toVector).toVector
    new SimpleMatrix(content)
  }

}
