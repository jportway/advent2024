import Day8.ta
import stain.TextMatrix

object Day8 {

  val ta = TextMatrix.apply(os.read.lines(os.pwd / "input" / "day8.txt"))

  val freqs = ta.locations.map(_.content).filterNot(_.contains('.')).map(_.get).distinct.toList
  val locs  = ta.find(x => x.contains('0'))

  def findAntiNodes(pair: List[ta.ValidLocation]): Seq[ta.ValidLocation] = {
    val a     = pair(0)
    val b     = pair(1)
    val diff  = a.vectorTo(b)
    val node1 = a + diff
    val node2 = b - diff
    Seq(node1, node2).collect { case a: ta.ValidLocation => a }
  }

  @main
  def day8main(): Unit = {
    println(s"frequencies:$freqs")
    val antiNodes = for {
      f         <- freqs
      locs       = ta.find(_.contains(f)).toList
      pair      <- locs.combinations(2)
      antiNodes <- findAntiNodes(pair)
    } yield antiNodes
    val distinct = antiNodes.distinct

    val viz = distinct.foldLeft(ta) { case (acc, n) =>
      acc.equivalentLocation(n).toOption.map(_.set('#')).getOrElse(acc)
    }
    viz.contents.foreach(println)

    println(distinct)
    println(distinct.length)
    //    println(locs.toList)
    println("done")
  }

}
