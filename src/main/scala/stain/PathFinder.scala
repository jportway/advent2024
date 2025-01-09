package stain

import com.barrybecker4.search.AStarSearch
import com.barrybecker4.search.space.AbstractSearchSpace
import stain.{Direction, Position, TextMatrix}

class Pathfinder(terrain: SpacialMatrix[?, ?], pathTest: (terrain.ValidCell) => Boolean) {

  import terrain.*
  def findPath(from: Position, to: Position): Option[Seq[Direction]] = {
    class MazeSearch(initialState: ValidCell, goalPos: ValidCell)
        extends AbstractSearchSpace[ValidCell, Direction](initialState) {

      override def legalTransitions(state: ValidCell): Seq[Direction] = {
        val allowedDirections = Direction.cardinals
        val valid = allowedDirections.filter(dir =>
          val newPos = state + dir
          newPos.isValid & pathTest(newPos.assertValid)
        )
        valid
      }

      override def getCost(state: ValidCell, move: Direction): Int = 1

      override def isGoal(state: ValidCell): Boolean = state == goalPos

      override def transition(state: ValidCell, move: Direction): ValidCell = (state + move).assertValid

      override def distanceFromGoal(state: ValidCell): Int =
        state.vectorTo(goalPos).size.toInt
    }

    def viz(path: List[ValidCell], map: TextMatrix): String = {
      val updates = path.map(p => (p, 'O'))
      map.update(updates).viz(c => c)
    }

    // ------------
    val initialState = terrain(from).assertValid
    val goalPos      = terrain(to).assertValid
    val space        = MazeSearch(initialState, goalPos)
    val search       = new AStarSearch[ValidCell, Direction](space)
    search.solve
  }

}
