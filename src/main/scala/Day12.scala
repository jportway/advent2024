import os.*
import stain.Direction
import stain.Position
import stain.SimpleMatrix
import stain.SpacialMatrix
import stain.TextMatrix
import stain.TextMatrix.*

object Day12 {

  case class CropRegion(crop: Char, contents: Set[TextCell] = Set.empty) {

    def area = contents.size

    lazy val perimeterCells = for {
      cell           <- contents.toList
      direction      <- Direction.cardinals
      surroundingCell = cell + direction
      if !contents.contains(surroundingCell)
    } yield (surroundingCell, direction)

    lazy val cost = perimeterCells.size * area

    /** find the number of sides by removing all but the rightmost or topmost perimeter cell in a side.
      *
      * So, for instance, if a perimeter cell is on the left hand side of the region, it will represent a vertical side
      * so then check if there are any cells in the perimeter above it, and if so delete them repeat this for all cells
      * in the perimeter and only the one on that side with nothing above it will remain in the list
      */
    lazy val sides = {
      val perim = perimeterCells.foldLeft(perimeterCells) { case (acc, (cell, direction)) =>
        val testDirection = direction match {
          case Direction.up | Direction.down    => Direction.right
          case Direction.left | Direction.right => Direction.up
        }
        val testValue         = (cell + testDirection, direction)
        val (beforeH, afterH) = acc.span(_ != testValue)
        beforeH ++ afterH.drop(1)
      }
      perim.map(_._1)
    }

    def fenceCost = sides.size * area

  }

  def findRegions(matrix: TextMatrix): Seq[CropRegion] = {
    val regs = matrix.connectedRegions(Direction.cardinals)((cell, fromCell, _) => cell.value == fromCell.value)
    regs.map(reg => CropRegion(reg.head.value, reg.toSet))
  }

  @main
  def day12Main(): Unit = {
    val map                = TextMatrix.fromStrings(os.read.lines(os.pwd / "input" / "Day12.txt"))
    val regions            = findRegions(map)
    val totalPerimeterCost = regions.map(_.cost).sum
    println(s"perimeter cost $totalPerimeterCost")
    val totalFenceCost = regions.map(_.fenceCost).sum
    println(s"fence cost $totalFenceCost")
  }

}
