object Day2 {

  def readFile(file: os.Path): Seq[Array[Int]] =
    os.read.lines(file).map(_.split(" ").map(_.toInt))

  def safe(list: Seq[Int]): Boolean = {
    def safeRecurse(list: Seq[Int], lastVal: Int, dir: Int): Boolean = {
      if (list.isEmpty) true
      else {
        val nextVal = list.head
        val dif = (nextVal - lastVal) * dir
        val failed = (dif > 3 || dif < 1)
        if (failed)
          false
        else
          safeRecurse(list.tail, nextVal, dir)
      }
    }
    val first = list.head
    val remaining = list.tail
    val initialDir = Math.signum(remaining.head - first).toInt
    safeRecurse(remaining, first, initialDir)
  }

  def variants(list: Seq[Int]): Seq[Seq[Int]] = {
    (0 to list.length).map { i =>
      list.take(i) ++ list.drop(i + 1)
    }
  }

  @main
  def day2Main(): Unit = {
    val testFile = readFile(os.pwd / "input" / "day2.txt")
    val tests = testFile.map(safe(_))
    val count = tests.count(_ == true)
    println(s"count of safe lists : $count")

    val mostlySafe = testFile.map(variants(_)).map(_.exists(safe)).count(_ == true)
    println(s"count of mostly safe : ${mostlySafe}")
  }

}
