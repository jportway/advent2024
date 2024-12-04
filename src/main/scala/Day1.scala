import os.{*, given}

object Day1 {
  @main
  def day1Main(): Unit = {
    val inputFile = os.pwd / "input" / "day1.txt"
    val listOfLists: Seq[IndexedSeq[Int]] =
      os.read.lines(inputFile).map(_.split("   ").map(_.toInt)).transpose
    val sortedLists = listOfLists.map(_.sorted)
    val list1 = sortedLists.head
    val list2 = sortedLists(1)
    val tuples: Seq[(Int, Int)] = list1.zip(list2)
    val diff = tuples.foldLeft(0) { case (acc, (a, b)) =>
      acc + Math.abs(b - a)
    }
    println(s"difference between lists : $diff")

    val commonCount = list1.foldLeft(0) { case (acc, elem) =>
      acc + (elem * list2.count(_ == elem))
    }

    println(s"total count of common elements : $commonCount")
  }

}
