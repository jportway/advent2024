import stain.Direction
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
    val f = Future.traverse(in.locations) { loc =>
      val newMaze            = loc.set('#') // put a barrier in current location
      val equivalentStart    = newMaze.equivalentLocation(startPos)
      val x: Future[Boolean] = Future(walk(newMaze, equivalentStart, Direction.up)).map(_.looped)
      x
    }
    val loops: Seq[Boolean] = Await.result(f, scala.concurrent.duration.Duration.Inf)
    val count               = loops.count(identity)
    println(s"number of loops: $count")
  }

  @tailrec
  private def step(maze: TextMatrix, loc: maze.Location, dir: Direction): (loc: maze.Location, dir: Direction) = {
    val newLoc = loc + dir
    if !newLoc.content.contains('#') then (newLoc, dir)
    else step(maze, loc, dir.turnRight)
  }

  private def walk(
      maze: TextMatrix,
      loc: maze.Location,
      dir: Direction
  ): (steps: List[(maze.Location, Direction)], looped: Boolean) = {

    @tailrec
    def _walk(
        currentLoc: maze.Location,
        currentDir: Direction,
        acc: List[(maze.Location, Direction)]
    ): (List[(maze.Location, Direction)], Boolean) = {
      val nextStep = step(maze, currentLoc, currentDir)
      nextStep match {
        case (newLoc, _) if !newLoc.isValid => (((currentLoc, currentDir) :: acc).reverse, false) // walked out
        case step if acc.contains(step)     => (((currentLoc, currentDir) :: acc).reverse, true)  // loop
        case (newLoc, newDir)               => _walk(newLoc, newDir, (currentLoc, currentDir) :: acc)
      }
    }
    _walk(loc, dir, List.empty)
  }

}
