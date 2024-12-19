import com.barrybecker4.search.AStarSearch
import com.barrybecker4.search.IDAStarSearch
import com.barrybecker4.search.space.AbstractSearchSpace
import com.barrybecker4.search.space.SearchSpace
import stain.Direction
import stain.TextMatrix

object Day16 {

  def parse(in: os.Path): TextMatrix = {
    val lines = os.read.lines(in).map(_.toIndexedSeq)
    TextMatrix(lines)
  }

  @main
  def day16main(): Unit = {

    val map      = parse(os.pwd / "input" / "day16.txt")
    val goalPos  = map.find(_ == 'E').head
    val startPos = map.find(_ == 'S').head

    import map.*

    case class State(
        pos: ValidCell,
        heading: Direction
    ) {

      def +(move: Direction): State =
        State((pos + move).getValid, move)

    }

    class MazeSearch(initialState: State, forbidden: Set[Direction])
        extends AbstractSearchSpace[State, Direction](initialState) {

      override def legalTransitions(state: State): Seq[Direction] = {
        val allowedDirections = if state == initialState then {
          Direction.cardinals.filterNot(forbidden.contains)
        } else {
          Direction.cardinals
        }
        val notWalls = allowedDirections.filterNot(dir => (state.pos + dir).contains('#'))
        notWalls
      }

      override def getCost(state: State, move: Direction): Int = cost(state.heading, move)

      override def isGoal(state: State): Boolean = state.pos == goalPos

      // when moving to a new state any existing forbidden move will disappear since that's only for the first move
      override def transition(state: State, move: Direction): State = state + move

      override def distanceFromGoal(state: State): Int =
        state.pos.vectorTo(goalPos).size.toInt
    }

    def viz(path: List[State], map: TextMatrix): String = {
      val updates = path.map(s => (s.pos, Direction.char(s.heading)))
      map.update(updates).viz(c => c)
    }

    def cost(oldHeading: Direction, move: Direction) = if (oldHeading == move) 1 else 1001

    // a path is a sequence of states along with their accumulated cost
    case class PathStep(state: State, accumulatedCost: Int)
    type Path = List[PathStep]

    /** creates a path from an initial state and a sequence of maves. the resulting path will inlucde the initial state
      */
    def pathFromSteps(start: State, moves: List[Direction]): Path = {
      val initialStep = PathStep(start, 0)
      val transformed = moves
        .foldLeft(List(initialStep))((pathList, move) =>
          val lastStep  = pathList.head
          val nextState = lastStep.state + move
          val nextCost  = lastStep.accumulatedCost + cost(lastStep.state.heading, move)
          PathStep(nextState, nextCost) :: pathList
        )
      transformed.reverse
    }

    val startState   = State(startPos, Direction.right)
    val space        = MazeSearch(startState, Set.empty)
    val search       = new AStarSearch[State, Direction](space)
    val optimumMoves = search.solve.get.toList
    val optimumPath  = pathFromSteps(startState, optimumMoves)
    val optimumCost  = optimumPath.last.accumulatedCost
    println(optimumCost)
    println(viz(optimumPath.map(_.state), map))

    // part 2
    // i should probably just write my own modified aStar .... but whatever....
    // we're going to search for a path from the start point, and then recursively search
    // for other possible paths from every point along the path,
    // except exact copies of this one (paths that take the same next step)
    case class SearchSeed(state: State, prequelCost: Int, forbiddenMoves: Set[Direction])
    val initialSeed = SearchSeed(startState, 0, Set.empty[Direction])
    val allPathResults: Seq[Option[Seq[State]]] = LazyList.unfold(List(initialSeed)) { toProcess =>
      if toProcess.isEmpty then None
      else {
        println(s"process queue length : ${toProcess.length}")
        val seed        = toProcess.head
        val remaining   = toProcess.tail
        val searchSpace = MazeSearch(seed.state, seed.forbiddenMoves)
        val foundMoves  = new AStarSearch(searchSpace).solve.map(_.toList)
        foundMoves match {
          case None => Some(None, remaining) // no valid path found
          case Some(moves) =>
            val path      = pathFromSteps(seed.state, moves)
            val totalCost = path.last.accumulatedCost + seed.prequelCost
            if totalCost > optimumCost then {
              Some(None, remaining) // no more optimum paths from this cell
            } else {
              val updatedCurrentSeed = Option // forbid the direction we used this time and try the seed again if necessary
                .when(seed.forbiddenMoves.size < 4)(
                  seed.copy(forbiddenMoves = seed.forbiddenMoves + moves.head)
                )
                .toList
              val newSearchSeeds = path.tail
                .zip(path.tail.tail)
                .map((step, nextStep) =>
                  SearchSeed(step.state, step.accumulatedCost + seed.prequelCost, Set(nextStep.state.heading))
                )
              val newSearches = updatedCurrentSeed ++ newSearchSeeds
              Some(Some(path.map(_.state)), newSearches ++ remaining)
            }
        }
      }
    }
    val allPaths = allPathResults.collect { case Some(x: Seq[State]) => x }
    println(viz(allPaths.flatten.toList, map))
    val allPathCells = allPaths.flatten.map(_.pos).toSet
    println(allPathCells.size)
  }

}
