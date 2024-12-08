import stain.Direction
import stain.Position
import stain.TextMatrix
import cats.*
import cats.given

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.Future
object Day6 {

  given ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  val in       = TextMatrix(os.read.lines(os.pwd / "input" / "day6.txt"))
  val startPos = in.find(_.exists(_ == '^')).head

  @main
  def Day6Main(): Unit = {
    val (steps, looped) = walk(in, startPos, Direction.up)
    println(s"looped: $looped")
    val uniqueLocs = steps.map(_._1).toSet
    println(s"unique locations visited: ${uniqueLocs.size}")
  }

  @main
  def Day6B(): Unit = {
    // just brute force it, it can't be that bad
    val start = System.currentTimeMillis()
    val f = Future.traverse(in.locations) { loc =>
      val newMaze            = loc.set('#')                   // put a barrier in current location
      val equivalentStart    = newMaze(startPos).toOption.get // i know it's valid
      val x: Future[Boolean] = Future(walk(newMaze, equivalentStart, Direction.up)).map(_.looped)
      x
    }
    val loops: Seq[Boolean] = Await.result(f, scala.concurrent.duration.Duration.Inf)
    val count               = loops.count(identity)
    val end                 = System.currentTimeMillis()
    println(s"number of loops: $count")
    println(s"time :${end - start}")
  }

  @tailrec
  private def step(
      maze: TextMatrix,
      loc: maze.Cell,
      dir: Direction
  ): (loc: maze.Cell, dir: Direction) = {
    val newLoc = loc + dir
    if !newLoc.content.contains('#') then (newLoc, dir)
    else step(maze, loc, dir.turnRight)
  }

  private def walk(
      maze: TextMatrix,
      loc: maze.ValidCell,
      dir: Direction
  ): (steps: List[(maze.ValidCell, Direction)], looped: Boolean) = {

    @tailrec
    def _walk(
        currentLoc: maze.ValidCell,
        currentDir: Direction,
        acc: List[(maze.ValidCell, Direction)]
    ): (List[(maze.ValidCell, Direction)], Boolean) = {
      val nextStep = step(maze, currentLoc, currentDir)
      nextStep match {
        case step if acc.contains(step)       => (((currentLoc, currentDir) :: acc).reverse, true)      // loop
        case (newLoc: maze.ValidCell, newDir) => _walk(newLoc, newDir, (currentLoc, currentDir) :: acc) // valid next step
        case _                                => (((currentLoc, currentDir) :: acc).reverse, false)     // walked out
      }
    }
    _walk(loc, dir, List.empty)
  }

}
