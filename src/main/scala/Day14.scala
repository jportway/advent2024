import stain.SimplePos
import stain.Direction
import stain.Position

import scala.math.*

object Day14 {

  case class Arena(numColumns: Int, numRows: Int, robots: Set[Robot]) {

    def moveRobots(seconds: Int): Arena = {
      val newRobots = robots.map { r =>
        val newPos = (r.pos + (r.vel * seconds)).wrap(numColumns, numRows)
        r.copy(pos = newPos)
      }
      copy(robots = newRobots)
    }

    def robotsInArea(tl: Position, br: Position): Set[Robot] = robots.filter { robot =>
      val p = robot.pos
      p.x >= tl.x & p.y >= tl.y & p.x < br.x & p.y < br.y
    }

    def quadrants: Seq[(SimplePos, SimplePos)] = {
      val yBreak = numRows / 2
      val xBreak = numColumns / 2
      val q1     = (SimplePos(0, 0), SimplePos(xBreak, yBreak))
      val q2     = (SimplePos(numColumns - xBreak, 0), SimplePos(numColumns, yBreak))
      val q3     = (SimplePos(0, numRows - yBreak), SimplePos(xBreak, numRows))
      val q4     = (SimplePos(numColumns - xBreak, numRows - yBreak), SimplePos(numColumns, numRows))
      List(q1, q2, q3, q4)
    }

    def quadrantCounts: Seq[Int] = quadrants.map(q => robotsInArea(q._1, q._2).size)

    def vis: String = {
      val lines = (0 until numRows).map { row =>
        (0 until numColumns)
          .foldLeft(List.empty[Char]) { (acc, col) =>
            val c = if robots.count(r => r.pos.x == col & r.pos.y == row) > 0 then 'O' else '.'
            c :: acc
          }
          .mkString
      }
      lines.mkString("\n")
    }

    /** return proportion of horizontally symmetrical points */
    def symmetry(): Double = {
      val symmetryCount = robots.count { r =>
        robots.exists(r2 => r2.pos.y == r.pos.y & r2.pos.x == (numColumns - 1) - r.pos.x)
      }
      symmetryCount.toDouble / robots.size.toDouble
    }

    /** return proportion of rpbots that have a neighbour one cell away */
    def neighbourliness(): Double = {
      val count = robots.count { r =>
        val neighbourPoints = r.pos.allNeighbours
        robots.exists { r2 =>
          neighbourPoints.contains(r2.pos)
        }
      }
      count.toDouble / robots.size.toDouble
    }

  }

  case class Robot(id: Int, pos: SimplePos, vel: Direction) {}

  @main
  def day14main(): Unit = {
    def parse(in: String) = {
      val regex = "p=(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)".r
      regex
        .findAllMatchIn(in)
        .zipWithIndex
        .map((m, i) =>
          Robot(i, SimplePos(m.group(1).toInt, m.group(2).toInt), Direction(m.group(3).toInt, m.group(4).toInt))
        )
    }

    val robots    = parse(os.read(os.pwd / "input" / "day14.txt"))
    val arena     = Arena(101, 103, robots.toSet)
    val afterMove = arena.moveRobots(100)
    println(afterMove.robots)
    val counts = afterMove.quadrantCounts
    counts.foreach(println)
    println(s"safety factor ${counts.product}")
    (1 to 1000000).foldLeft(arena) { (acc, cur) =>
      val nextArena = acc.moveRobots(1)
      if (nextArena.neighbourliness() > 0.6) {
        println(s"time = $cur")
        println(nextArena.vis)
        println("\n\n")
      }
      nextArena
    }
  }

}
