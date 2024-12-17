import stain.Direction
import stain.TextMatrix

import scala.::

object Day15 {

  case class Arena(tm: TextMatrix, robot: TextMatrix#ValidCell) {

    def move(direction: Direction) = {
      // if moving vertically, recursively find all cells that will move
      def stackVertical(pos: tm.ValidCell): Option[Set[tm.ValidCell]] = {
        pos.value match {
          case '#' => None // can't move something hit a wall
          case '.' => Some(Set.empty)
          case 'O' => (pos + direction).toOption.flatMap(stackVertical).map(_ + pos)
          case '[' => // in the case of big boxes we have to move the other half of the box too
            val other = (pos + Direction.right).getValid
            val a     = stackVertical((pos + direction).getValid).map(_ + pos)
            val b     = stackVertical((other + direction).getValid).map(_ + other)
            a.zip(b).map((am, bm) => am ++ bm)
          case ']' =>
            val other = (pos + Direction.left).getValid
            val a     = stackVertical((pos + direction).getValid).map(_ + pos)
            val b     = stackVertical((other + direction).getValid).map(_ + other)
            a.zip(b).map((am, bm) => am ++ bm)
        }
      }

      def stackHorizontal(pos: tm.ValidCell): Option[Set[tm.ValidCell]] = {
        pos.value match {
          case '.'             => Some(Set.empty)
          case '[' | ']' | 'O' => (pos + direction).toOption.flatMap(stackHorizontal).map(_ + pos)
          case '#'             => None
        }
      }

      val newPos = tm(robot + direction).getValid
      val toMove = direction match { // make a list of anything that needs to move
        case Direction.left | Direction.right => stackHorizontal(newPos)
        case Direction.up | Direction.down    => stackVertical(newPos)
      }
      toMove match {
        case None => this // can't move, nothing changes
        case Some(cellsToMove) =>
          val clear        = cellsToMove.toList.map(x => (x, '.'))
          val clearedState = tm.update(clear)            // delete boxes to be moved
          val redraw       = cellsToMove.toList.map(x => ((x + direction).getValid, x.value))
          val arenaState   = clearedState.update(redraw) // redraw boxes in new position
          copy(arenaState, newPos)
      }
    }

    def gpsSum = {
      val boxes = tm.find(c => c == 'O' | c == '[')
      boxes.map(b => b.x + b.y * 100).sum
    }

    def viz: String = tm.update(robot, '@').viz(c => c)

  }

  def parseMap(lines: Seq[String]): Arena = {
    val indexedLines = lines.toIndexedSeq
    val matrix       = TextMatrix.fromStrings(indexedLines)
    val robot        = matrix.find(_ == '@').head
    val robotRemoved = matrix.update(robot, '.')
    Arena(robotRemoved, robot)
  }

  def parseMoves(lines: Seq[String]): Seq[Direction] = {
    val string = lines.mkString
    string.map {
      case '<' => Direction.left
      case '>' => Direction.right
      case '^' => Direction.up
      case 'v' => Direction.down
    }
  }

  def parse(in: Seq[String]): (Arena, Seq[Direction]) = {
    val (mapStr, moveStr) = in.span(_.nonEmpty)
    (parseMap(mapStr), parseMoves(moveStr))
  }

  def stretchMap(arena: Arena) = {
    val bigRows = arena.tm.rowContents
      .map(row =>
        row.flatMap {
          case '#' => Seq('#', '#')
          case 'O' => Seq('[', ']')
          case '.' => Seq('.', '.')
          case '@' => Seq('@', '.')
        }
      )
      .toIndexedSeq
    val matrix = TextMatrix(bigRows)
    val robot  = matrix(arena.robot.x * 2, arena.robot.y).toOption.get
    Arena(matrix, robot)
  }

  @main
  def day15Main(): Unit = {
    val in = os.read.lines(os.pwd / "input" / "Day15.txt")

    val (map, moves) = parse(in)
    executeMovesAndPrintResult(map, moves)

    val bigMap = stretchMap(map)
    executeMovesAndPrintResult(bigMap, moves)

  }

  private def executeMovesAndPrintResult(map: Arena, moves: Seq[Direction]): Unit = {
    val finalMap = moves.foldLeft(map) { (acc, move) =>
      val newAcc = acc.move(move)
      println(newAcc.viz)
      newAcc
    }

    println(finalMap.gpsSum)
    println("\n\n\n")
  }

}
