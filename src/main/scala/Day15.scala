import stain.Direction
import stain.TextMatrix

import scala.::

object Day15 {

  case class Arena(tm: TextMatrix, robot: TextMatrix#ValidCell) {

    def move(direction: Direction) = {
      // assume pos is empty
      def stack(pos: tm.ValidCell): Option[List[tm.ValidCell]] = {
        pos.value match {
          case '.' => Some(List.empty)
          case 'O' => (pos + direction).toOption.flatMap(stack).map(pos :: _)
          case '#' => None
        }
      }
      val newPos = tm(robot + direction).toOption.get
      val toMove = stack(newPos)
      toMove match {
        case None => this // can't move, nothing changes
        case Some(moves) =>
          val updates    = (newPos, '.') :: moves.map(x => (x + direction, x.value))
          val arenaState = tm.update(updates)
          copy(arenaState, newPos)
      }
    }

    def gpsSum = {
      val boxes = tm.find(_ == 'O')
      boxes.map(b => b.x + b.y * 100).sum
    }

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

  @main
  def day15Main(): Unit = {
    val in           = os.read.lines(os.pwd / "input" / "Day15.txt")
    val (map, moves) = parse(in)
    val finalMap = moves.foldLeft(map) { (acc, move) =>
      acc.move(move)
    }
    println(finalMap.tm.viz(x => x))
    println(finalMap.gpsSum)
  }

}
