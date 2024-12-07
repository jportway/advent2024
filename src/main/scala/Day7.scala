import scala.annotation.tailrec
import cats.syntax.all
import cats.instances.all

object Day7 {

  @main
  def day7Main(): Unit =
    val lines = os.read.lines(os.pwd / "input" / "day7.txt")
    val puzzles = for {
      line  <- lines
      parts  = line.split(":")
      target = parts.head.toLong
      nums   = parts(1).split(" ").map(_.trim).filter(_.nonEmpty).map(_.toLong).toVector
    } yield (target, nums)

    def doOperations(opCode: Long, numList: Vector[Long]): Long = {
      val (result, _) = numList.tail.foldLeft((numList.head, opCode)) { case ((acc, op: Long), v) =>
        val operator = op & 1
        val nextOp   = op >>> 1
        if (operator == 0)
          (acc + v, nextOp)
        else
          (acc * v, nextOp)
      }
      result
    }

    val res = puzzles.map { case (target, nums) =>
      val max = BigDecimal(2L).pow(nums.length - 1).toLongExact // number of possible variations
      val str = for {
        i <- 0L until max // all variations
        v  = doOperations(i, nums)
        if v == target
      } yield v
      if (str.nonEmpty) target else 0
    }.sum

    println(s"sum = $res")

}
