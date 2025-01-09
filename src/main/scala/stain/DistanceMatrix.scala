package stain

/** a SpacialMatrix in which every location contains the distance to a goal Again, this would be much more efficiently
  * implemented using a custom A* but I can't be bothered
  */
type DistanceMatrix = SimpleMatrix[Option[Int]]
object DistanceMatrix {

  def from(
      terrain: SpacialMatrix[?, ?],
      goal: terrain.ValidCell,
      pathTest: terrain.ValidCell => Boolean
  ): DistanceMatrix = {
    val distanceArray = Array.fill[Option[Int]](terrain.numRows, terrain.numColumns)(None)
    val pathfinder    = Pathfinder(terrain, pathTest)

    terrain.locations.filter(pathTest).foreach { loc =>
      if (distanceArray(loc.y)(loc.x).isEmpty) { // don't bother recalculating if we've already calculated this cell as part of another path
        val path = pathfinder.findPath(loc, goal)
        path.foreach { pathSteps =>
          distanceArray(loc.y)(loc.x) = Some(pathSteps.size)
          // walk the path, filling in the distance array with the distance at each step
          pathSteps.zipWithIndex.foldLeft(loc) { case (loc, (stp, dist)) =>
            val newLoc   = (loc + stp).assertValid
            val distance = pathSteps.size - (dist + 1)
            distanceArray(newLoc.y)(newLoc.x) = Some(distance)
            newLoc
          }
        }
      }
    }
    val immutableMatrix = distanceArray.map(_.toVector).toVector
    SimpleMatrix(immutableMatrix)
  }

}
