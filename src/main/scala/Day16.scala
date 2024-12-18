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

    case class State(pos: ValidCell, heading: Direction)
    case class Move(fromHeading: Direction, toHeading: Direction) {
      def cost = if (fromHeading == toHeading) 1 else 1001
    }

    class MazeSearch(initialState: State) extends AbstractSearchSpace[State, Move](initialState) {

      override def legalTransitions(state: State): Seq[Move] =
        Direction.cardinals.filter(dir => !(state.pos + dir).contains('#')).map(Move(state.heading, _))

      override def getCost(move: Move): Int = move.cost

      override def isGoal(state: State): Boolean = state.pos == goalPos

      override def transition(state: State, move: Move): State =
        State((state.pos + move.toHeading).getValid, move.toHeading)

      override def distanceFromGoal(state: State): Int =
        state.pos.vectorTo(goalPos).size.toInt
    }

    val space  = MazeSearch(State(startPos, Direction.right))
    val search = new AStarSearch[State, Move](space)
    val path   = search.solve
    println(path)
    val totalCost = path.map(_.map(_.cost).sum)
    println(totalCost)
  }

}
