package stain

import com.barrybecker4.search.AStarSearch
import com.barrybecker4.search.space.AbstractSearchSpace
import com.barrybecker4.search.space.SearchSpace

object AllPaths {

  // This is probably dumb and very ineffictent and it would be better to modify th A* itself
  // but this will have to do for now.
  // this will find all paths to a goal in a given space that satisfy a path filter
  // we have an extra path filter here, because it may be necessary to implement a rule to constrain the number of
  // paths produced.
  def find[S, T](
      initialState: S,
      space: (start: S) => SearchSpace[S, T],
      pathFilter: (path: Seq[S]) => Boolean
  ) = {

    type Path = List[S]

    val initialSpace = space(initialState)
    val initialSeed: Set[Path] =
      initialSpace.legalTransitions(initialState).map(t => List(initialSpace.transition(initialState, t))).toSet

    val allPathResults: Seq[Option[Seq[S]]] = LazyList.unfold(initialSeed, Set.empty[Path]) { (toProcess, processed) =>
      if toProcess.isEmpty then None // we're done
      else {
        println(s"queue length :${toProcess.size}")
        val seedPath    = toProcess.head // seed path is the path that ends in the state we're searching
        val searchStart = seedPath.last  // state we're searching from
        val remaining   = toProcess.tail
        if (!processed.contains(seedPath)) {
          val updatedProcessed = processed + seedPath
          val searchSpace      = space(searchStart)
          val searchResult     = new AStarSearch(searchSpace).solve.map(_.toList)
          searchResult match {
            case None => Some(None, (remaining, updatedProcessed)) // no valid path found
            case Some(moves) =>
              val foundPath =
                moves
                  .foldLeft(List(searchStart))((acc, move) => searchSpace.transition(acc.head, move) :: acc)
                  .reverse
              val resultPath = seedPath ++ (foundPath.tail)
              if (pathFilter(resultPath)) {
                val (newSearchPaths, _) = moves.foldLeft((List.empty[Path], seedPath.reverse)) {
                  case ((out, accState), move) =>
                    val currentState = accState.head
                    val otherMoves   = searchSpace.legalTransitions(currentState).filterNot(_ == move)
                    val newSearchPaths =
                      otherMoves.map(x => (searchSpace.transition(currentState, x) :: accState).reverse)
                    val newState = searchSpace.transition(currentState, move)
                    (out ++ newSearchPaths, newState :: accState)
                }
                Some(Some(resultPath), (newSearchPaths.toSet ++ remaining, updatedProcessed))
              } else {
                Some(None, (remaining, updatedProcessed)) // no more optimum paths from this state
              }
          }
        } else {
          Some(None, (remaining, processed))
        }
      }
    }
    allPathResults.collect { case Some(path) => path }
  }

}
