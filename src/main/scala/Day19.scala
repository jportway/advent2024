import com.barrybecker4.search.AStarSearch
import com.barrybecker4.search.space.AbstractSearchSpace

object Day19 {

  def parse(in: Seq[String]) = {
    val regex    = """([A-z]+)""".r
    val towels   = regex.findAllMatchIn(in.head).map(_.group(1)).toList
    val patterns = in.tail.tail.toList
    (towels, patterns)
  }

  class SearchSpace(goal: String, towels: Seq[String]) extends AbstractSearchSpace[String, String]("") {

    override def isGoal(state: String): Boolean = state == goal

    override def legalTransitions(state: String): Seq[String] = {
      val remaining = goal.drop(state.length)
      towels.filter(remaining.startsWith)
    }

    override def transition(state: String, transition: String): String = state + transition
    override def distanceFromGoal(state: String): Int                  = goal.length - state.length
    override def getCost(state: String, transition: String): Int       = 1

  }

  @main
  def day19Main(): Unit = {
    val in                 = os.pwd / "input" / "day19.txt"
    val (towels, patterns) = parse(os.read.lines(in))

    val results = patterns.map { pattern =>
      val space  = SearchSpace(pattern, towels)
      val search = new AStarSearch(space)
      search.solve
    }

    println(results.count(_.nonEmpty))
  }

}
