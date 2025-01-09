import stain.*

object Day20 {

  @main
  def day20Main(): Unit = {
    val lines  = os.read.lines(os.pwd / "input" / "day20.txt")
    val source = TextMatrix.fromStrings(lines)
    val s      = source.find(_ == 'S').head
    val e      = source.find(_ == 'E').head
    val maze   = source.update(Seq((s, '.'), (e, '.')))
    val start  = maze(s).assertValid
    val end    = maze(e).assertValid

    println("calculating distance matrix")
    // create a maze with distances at each valid corridor location, wall locations will contain None
    val distanceMatrix = DistanceMatrix.from(maze, end, (x: maze.ValidCell) => x.contains('.'))

    type CheatValue = Int
    type CheatCount = Int
    case class Cheat(endpoint: distanceMatrix.ValidCell, value: CheatValue)
    case class CheatSet(location: distanceMatrix.ValidCell, cheats: Seq[Cheat])

    // find all possible cheats on the map of length less than the searchRadius
    // returning a list of locations and the cheats from that location
    // each cheat is specified by its endpoint and the time saved by the cheat
    // this returns a lazyList (stream) since I'm expecting a LOT of cheats for part 2
    def findCheats(searchRadius: Int): Seq[CheatSet] = {
      println("calculating cheat matrix")
      // for every cell that is part of a path we search around it for possible cheats
      // and return a sequence of the distance saved by each cheat
      distanceMatrix.locations.map { loc =>
        loc.value.map { locDist => // current distance to goal at this location
          // now we search every cell within a manhattan distance < searchRadius
          // and calculate the cheat values
          val cheats = for {
            x <- -searchRadius to searchRadius
            y <- -searchRadius to searchRadius
            if x != 0 | y != 0 // don't search the same location
            cellsTravelled = math.abs(x) + math.abs(y)
            if cellsTravelled <= searchRadius
            p = distanceMatrix(loc.x + x, loc.y + y)
            v = p.content.flatten // None if location is outside the matrix or is a wall
            if v.nonEmpty
            dif = (locDist - v.get) - cellsTravelled
            if dif > 0
          } yield Cheat(p.assertValid, dif)
          CheatSet(loc, cheats)
        }.getOrElse(CheatSet(loc, Seq.empty))
      }
    }

    // reduces the stream of cheats into a map which counts the number of occurrences of each cheat value
    def getDistances(cheats: Seq[CheatSet], minValue: CheatValue): Map[CheatValue, CheatCount] = {
      // we extract only the cheat values because we aren't using the locations, and filter out any cheats that are too small
      val cheatDistances = cheats.flatMap(_.cheats).map(x => x.value).filter(_ >= minValue)
      val groupedByValue = cheatDistances.groupMapReduce(identity)(_ => 1)((acc, x) => acc + x)
      groupedByValue
    }

    def printSortedCheats(cheatGroups: Map[CheatValue, CheatCount]) = {
      val sorted = cheatGroups.toList.sortBy(_._1)
      sorted.foreach { (value, occurrences) =>
        println(s"cheat value $value occurs $occurrences times")
      }
    }

    val partACheats  = findCheats(searchRadius = 2)
    val partAResults = getDistances(partACheats, minValue = 100)
    printSortedCheats(partAResults)
    println(s"part A : ${partAResults.values.sum}")

    val partBCheats  = findCheats(searchRadius = 20)
    val partBResults = getDistances(partBCheats, 100)
    println(s"part B : ${partBResults.values.sum}")

  }

}
