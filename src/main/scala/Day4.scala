import stain.Direction
import stain.TextMatrix

import scala.annotation.targetName

object Day4 {

  @main
  def test(): Unit = {

    // for each location in the matrix, check all directions for the given string and count how many instances you find
    def findWords(input: TextMatrix, searchString: String): Int = input.locations.foldLeft(0) { (acc, loc) =>
      val numFound = Direction.allDirections.count(dir => input.stringCheck(searchString, loc, dir))
      acc + numFound
    }

    val in = TextMatrix(os.read.lines(os.pwd / "input" / "day4.txt"))

    println(s"number of XMAS : ${findWords(in, "XMAS")}")

    val midpoints = in.locations.filter(_.value == 'A')
    val X_mas = midpoints.count { loc =>
      // check all 4 diagonals - a valid X_MAS will have 2 diagonals with MAS
      Direction.diagonals.count { dir =>
        in.stringCheck("MAS", loc + dir, -dir)
      } == 2
    }

    println(s"number of X_MAS : $X_mas")
  }

}
