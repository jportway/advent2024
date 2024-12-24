import com.barrybecker4.search.AStarSearch
import com.barrybecker4.search.space.AbstractSearchSpace
import stain.AllPaths

import scala.collection.immutable.Queue
import scala.concurrent.{Await, Future}

object Day19 {

  type Towel   = String
  type Pattern = String
  type Path    = Vector[Towel]

  def parse(in: Seq[String]): (Set[Towel], List[Pattern]) = {
    val regex    = """([A-z]+)""".r
    val towels   = regex.findAllMatchIn(in.head).map(_.group(1)).toSet
    val patterns = in.tail.tail.toList
    (towels, patterns)
  }

  class SearchSpace(initialState: String, goal: String, towels: Set[String])
      extends AbstractSearchSpace[String, String](initialState) {

    override def isGoal(state: String): Boolean = state == goal

    override def legalTransitions(state: String): Seq[String] = {
      val remaining = goal.drop(state.length)
      towels.filter(remaining.startsWith).toSeq
    }

    override def transition(state: String, transition: String): String = state + transition
    override def distanceFromGoal(state: String): Int                  = goal.length - state.length
    override def getCost(state: String, transition: String): Int       = 1

  }

  @main
  def day19Main(): Unit = {
    val in = os.pwd / "input" / "day19.txt"

    val (towels, patterns) = parse(os.read.lines(in))

    val results = patterns.map { pattern =>
      val space  = SearchSpace("", pattern, towels)
      val search = new AStarSearch(space)
      search.solve
    }

    println(s"part A : ${results.count(_.nonEmpty)}")

    given ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    val work = Future.traverse(patterns) { goal =>
      Future(countAllPaths(goal, towels.toList))
    }

    val pathCounts: Seq[Int] = Await.result(work, scala.concurrent.duration.Duration.Inf)

    println(s"Part B : total possible paths : ${pathCounts.sum}")

  }

  extension (path: Path) {

    inline def matches(pattern: Pattern): Boolean =
      toPattern == pattern

    inline def toPattern = path.mkString

    inline def findPossibleSteps(goal: Pattern, towels: List[Towel]): List[Path] = {
      val remaining = goal.drop(path.toPattern.length)
      val nextSteps = towels.filter(remaining.startsWith)
      nextSteps.map(towel => path.appended(towel)).toList
    }

  }

  // depth first search of all possible paths.
  // at each step we make a list of any possible subsequent steps and then follow the first
  // until it either reaches the goal or we can't make another step, then go back to the
  // last queued step. We can probably do this recursively because it won't get that deep
  // but maybe it would be better to use a lazylist for state management instead. Don't think we need
  // to build an actual graph structure - just a queue of unprocessed branches.
  private def countAllPaths(goal: String, towels: List[String]) = {
    val start                = System.currentTimeMillis()
    val initialPossibilities = List(Vector.empty[Towel])
    val pathList = LazyList
      .unfold[Int, List[Path]](initialPossibilities) { queue =>
        if queue.isEmpty then None
        else {
          val currentPath = queue.head
          val remaining   = queue.tail
          if (currentPath.matches(goal)) {
            Some(1, remaining)
          } else {
            val nextSteps = currentPath.findPossibleSteps(goal, towels)
            Some(0, nextSteps ++ remaining)
          }
        }
      }
    val pathCount = pathList.count(_ == 1)
    val end       = System.currentTimeMillis()
    println(s"number of possible paths for $goal = $pathCount - time to calculate = ${end - start}")
    pathCount
  }

}
