import com.barrybecker4.search.AStarSearch
import com.barrybecker4.search.space.AbstractSearchSpace
import stain.AllPaths

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.concurrent.{Await, Future}

object Day19 {

  type Towel   = String
  type Pattern = String

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
      val result = search.solve
      if (result.isEmpty) println(s"$pattern - impossible")
      result
    }

    println(s"part A : ${results.count(_.nonEmpty)}")

    val graph = patterns.map { pattern =>
      val graph = patternToDag(pattern, towels)
      val paths = countPaths(graph, pattern)
      println(s"pattern : $pattern  - count :$paths")
      paths
    }
    println(s"part B :${graph.sum}")
  }

  // we can treat all the possible paths through the pattern as a directed graph.
  // where there are two possible ways of proceeding from a particular state in the pattern
  // it will be a branch in the graph. Then we can sum all the possible branches through the
  // graph. We can do this by just recursively counting all paths
  // it turns out there are a *lot* of paths, so we have to cache the path counts - this will save
  // us recalculating all of the later branches for every early branch, which should make
  // the calculation manageable i think

  trait DagNode[T] {

    def id: T
    def children: Set[DagNode[T]]

  }

  def countPaths[T](root: DagNode[T], goal: T): Long = {
    val cache = scala.collection.mutable.Map.empty[T, Long] // mutable is ok. really.

    def recurse(n: DagNode[T]): Long = {
      cache.getOrElseUpdate( // is getOrElseUpdate ok with recursive calls ??
        n.id,
        if n.id == goal then 1L
        else if n.children.isEmpty then 0L
        else n.children.toList.map(recurse).sum
        // don't think it'll matter that this isn't tail recursive,
        // since the maximum depth will be the number of towels in a single pattern
      )
    }
    recurse(root)
  }

  // translates the search for patterns into a directed acyclic graph
  def patternToDag(goal: Pattern, towels: Set[Towel]): DagNode[Pattern] = {
    case class PatternDagNode(state: String) extends DagNode[Pattern] {
      def id = state

      lazy val children: Set[DagNode[Pattern]] = {
        val remaining = goal.drop(state.length)
        towels.filter(remaining.startsWith).map(towel => PatternDagNode(state + towel))
      }

    }
    PatternDagNode("")
  }

}
