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

      class MazeSearch(initialState: ValidCell, goalPos: ValidCell)
          extends AbstractSearchSpace[ValidCell, Direction](initialState) {

        override def legalTransitions(state: ValidCell): Seq[Direction] = {
          val allowedDirections = Direction.cardinals
          val valid = allowedDirections.filterNot(dir =>
            val newPos = state + dir
            (!newPos.isValid) | newPos.contains('#')
          )
          valid
        }

        override def getCost(state: ValidCell, move: Direction): Int = 1

        override def isGoal(state: ValidCell): Boolean = state == goalPos

        override def transition(state: ValidCell, move: Direction): ValidCell = (state + move).getValid

        override def distanceFromGoal(state: ValidCell): Int =
          state.vectorTo(goalPos).size.toInt
      }

      def viz(path: List[ValidCell], map: TextMatrix): String = {
        val updates = path.map(p => (p, 'O'))
        map.update(updates).viz(c => c)
      }

      // ------------
      val initalState = terrain(0, 0).getValid
      val goalPos     = terrain(70, 70).getValid
      val space       = MazeSearch(initalState, goalPos)
      val search      = new AStarSearch[ValidCell, Direction](space)
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
