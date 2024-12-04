import munit.{*, given}
import os.{*, given}

class Day2Suite extends FunSuite {

  test("day2A") {
    val testFile = os.pwd / "input" / "day2test.txt"
    val obtained = Day2.readFile(testFile).map(Day2.safe(_))
    val expected = Seq(true, false, false, false, false, true)
    assertEquals(obtained, expected)
  }

  test("day2B") {
    Day2.variants(Seq(1, 2, 3, 4)).foreach(println)
    val testFile = os.pwd / "input" / "day2test.txt"
    val obtained =
      Day2.readFile(testFile).map(Day2.variants(_)).map(_.exists(Day2.safe))
    val expected = Seq(true, false, false, true, true, true)
    println ("number of safe : " + obtained.count(_ == true))
    assertEquals(obtained, expected)
    
  }

}
