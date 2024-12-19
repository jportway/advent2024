import com.barrybecker4.search.AStarSearch
import com.barrybecker4.search.space.AbstractSearchSpace
import stain.Direction
import stain.SimplePos
import stain.TextMatrix

object Day18 {

  def parse(lines: String) = {
    val regex = """(\d+),(\d+)""".r
    regex.findAllMatchIn(lines).map(m => SimplePos(m.group(1).toInt, m.group(2).toInt))
  }

  @main
  def day18Main(): Unit = {

    def findPath(terrain: TextMatrix): Option[Seq[Direction]] = {
      import terrain.*

      case class State(pos: ValidCell, heading: Direction) {
        def +(move: Direction): State = State((pos + move).getValid, move)
      }

      class MazeSearch(initialState: State, goalPos: ValidCell)
          extends AbstractSearchSpace[State, Direction](initialState) {

        override def legalTransitions(state: State): Seq[Direction] = {
          val allowedDirections = Direction.cardinals
          val valid = allowedDirections.filterNot(dir =>
            val newPos = state.pos + dir
            (!newPos.isValid) | newPos.contains('#')
          )
          valid
        }

        override def getCost(state: State, move: Direction): Int = 1

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

      // ------------
      val initalState = State(terrain(0, 0).getValid, Direction.down)
      val goalPos     = terrain(70, 70).getValid
      val space       = MazeSearch(initalState, goalPos)
      val search      = new AStarSearch[State, Direction](space)
      search.solve
    }

    val in     = os.read(os.pwd / "input" / "Day18.txt")
    val blocks = parse(in).toSeq

    val empty   = TextMatrix(Vector.fill(71, 71)(' '))
    val fallen  = blocks.take(1024).map((_, '#')).toList
    val map1024 = empty.update(fallen)
    val path    = findPath(map1024).get
    println(s"part A :${path.size - 1}")

    val failsAt = LazyList
      .unfold(1024) { numBlocks =>
        val falling      = blocks.take(numBlocks).map((_, '#')).toSeq
        val terrain      = empty.update(falling)
        val searchResult = findPath(terrain)
        searchResult match {
          case Some(path) =>
            println(s"search :$numBlocks = ${path.size - 1}")
            Some(numBlocks, numBlocks + 1)
          case None =>
            println(s"found block at $numBlocks")
            None
        }
      }
      .last
    
    println(s"part B : ${blocks(failsAt)}")
  }

}
