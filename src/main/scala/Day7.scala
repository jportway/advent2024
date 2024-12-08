import scala.concurrent.Await
import scala.concurrent.Future

object Day7 {

  given ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  @main
  def day7Main(): Unit =
    val lines = os.read.lines(os.pwd / "input" / "day7.txt")
    val puzzles = for {
      line  <- lines
      parts  = line.split(":")
      target = parts.head.toLong
      nums   = parts(1).split(" ").map(_.trim).filter(_.nonEmpty).map(_.toLong).toVector
    } yield (target, nums)

    def doOperations(ops: List[Char], numList: Vector[Long]): Long = {
      val (result, _) = numList.tail.foldLeft((numList.head, ops)) { case ((acc, op: List[Char]), v) =>
        val operator  = op.head
        val remaining = op.tail
        operator match {
          case '+' => (acc + v, remaining)
          case '*' => (acc * v, remaining)
          case '|' => ((acc.toString + v.toString).toLong, remaining)
        }
      }
      result
    }

    /** lazily produce every possible combination of operators */
    def allSequencesLazy(n: Int, ops: List[Char]): LazyList[List[Char]] = {
      val lazyOps = ops.to(LazyList)
      (1 to n).foldLeft(LazyList(List.empty[Char])) { (acc, _) =>
        for {
          seq <- acc
          op  <- lazyOps
        } yield seq :+ op
      }
    }

    def solvePuzzles(puzzles: Seq[(Long, Vector[Long])], ops: List[Char]): Long = {
      println("calculating puzzles")
      val futures = Future
        .traverse(puzzles) { case (target, nums) =>
          Future {
            val allVariants = allSequencesLazy(nums.length, ops)
            val str = allVariants.find { ops =>
              doOperations(ops, nums) == target
            }
            if (str.nonEmpty) target else 0
          }
        }
      Await.result(futures, scala.concurrent.duration.Duration.Inf).sum
    }

    val answerA = solvePuzzles(puzzles = puzzles, ops = List('+', '*'))
    println(s"answer A = $answerA")

    val answerB = solvePuzzles(puzzles = puzzles, ops = List('+', '*', '|'))
    println(s"answer B = $answerB")

}
