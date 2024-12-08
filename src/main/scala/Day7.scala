import scala.annotation.tailrec
import cats.syntax.all
import cats.instances.all

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

    // this is incredibly shit and i should do this lazily with a fold, but needs must...
    def recursiveBuild(depth: Int, values: List[Char]): List[List[Char]] = {
      if (depth == 0) {
        values.map(List(_))
      } else {
        recursiveBuild(depth - 1, values).flatMap(x => values.map(y => y :: x)) // not tailrec, but it's ok
      }
    }

    val longestSequence = puzzles.maxBy { case (_, nums) => nums.length }._2.length
    println("building cache")
    val cache = (for {
      i    <- 2 to longestSequence
      opSeq = recursiveBuild(i - 1, List('+', '*', '|'))
    } yield i -> opSeq).toMap

    println("calculating puzzles")
    val futures = Future
      .traverse(puzzles) { case (target, nums) =>
        Future {
          val allVariants = cache(nums.length)
          val str = allVariants.find{ops =>
            doOperations(ops, nums) == target
          }
          if (str.nonEmpty) target else 0
        }
      }
    val res = Await.result(futures, scala.concurrent.duration.Duration.Inf).sum

    println(s"sum = $res")

}
