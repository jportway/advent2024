import Day8.ta
import stain.TextMatrix

object Day8 {

  val ta = TextMatrix.fromStrings(os.read.lines(os.pwd / "input" / "day8.txt"))

  val freqs = ta.locations.map(_.content).filterNot(_.contains('.')).map(_.get).distinct.toList

  def findResonantAntiNodes(pair: List[ta.ValidCell]): Seq[ta.ValidCell] = {
    val a        = pair(0)
    val b        = pair(1)
    val diff     = a.vectorTo(b)
    val forward  = List.unfold(a)(loc => (loc + diff).toOption.map(newLoc => (newLoc, newLoc)))
    val backward = List.unfold(b)(loc => (loc - diff).toOption.map(newLoc => (newLoc, newLoc)))
    pair ++ forward ++ backward
  }

  def findAntiNodes(pair: List[ta.ValidCell]): Seq[ta.ValidCell] = {
    val a     = pair(0)
    val b     = pair(1)
    val diff  = a.vectorTo(b)
    val node1 = a + diff
    val node2 = b - diff
    Seq(node1, node2).collect { case a: ta.ValidCell => a }
  }

  def findAll(findFunction: List[ta.ValidCell] => Seq[ta.ValidCell]) = {
    val antiNodes = for {
      f         <- freqs
      locs       = ta.find(_ == f).toList
      pair      <- locs.combinations(2)
      antiNodes <- findFunction(pair)
    } yield antiNodes
    antiNodes.distinct
  }

  private def printVisualization(antiNodes: List[ta.ValidCell]): Unit = {
    val updates = antiNodes.map(pos => pos -> '#')
    val viz     = ta.update(updates)
    viz.contents.foreach(println)
  }

  @main
  def day8main(): Unit = {

    val partA = findAll(findAntiNodes)
    printVisualization(partA)
    println(s"part A :${partA.length}")

    println("\n\n\n\n")

    val partB = findAll(findResonantAntiNodes)
    printVisualization(partB)
    println(s"part B :${partB.length}")
  }

}
