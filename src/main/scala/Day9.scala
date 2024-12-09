import Day9.DiskChunk.End
import Day9.DiskChunk.FileChunk
import Day9.DiskChunk.FreeSpace
import os.*
object Day9 {

  enum DiskChunk {

    case FileChunk(id: Int, length: Int)
    case FreeSpace(length: Int)
    case End

  }
  def parse(in: String): Seq[DiskChunk] = {
    val inStream = (in + "0").grouped(2).zipWithIndex.toList
    println(inStream)
    val mappedStream =
      inStream.flatMap((str, id) => List(FileChunk(id, str(0) - '0'), FreeSpace(str(1) - '0'))).toVector
    val layout: Seq[DiskChunk] = mappedStream.collect {
      case a: FileChunk                 => a
      case s: FreeSpace if s.length > 0 => s
    } // remove zero length gaps
    layout
  }

  def fillGap(
      spaceToFill: FreeSpace,
      file: FileChunk,
      queue: Seq[DiskChunk],
      supply: Seq[FileChunk]
  ): (FileChunk, DiskChunk, FileChunk, Seq[DiskChunk], Seq[FileChunk]) = {
    val filled         = Math.min(spaceToFill.length, file.length)
    val remainingFile  = file.length - filled
    val remainingSpace = spaceToFill.length - filled
    val (newFiller, newSupply) =
      if remainingFile > 0 then {
        val x: (FileChunk, Seq[DiskChunk.FileChunk]) = (FileChunk(file.id, remainingFile), supply) // ??? compiler bug - not finding type correctly???
        x
      } else (supply.head, supply.tail)
    val (newHead, newQueue) =
      if remainingSpace > 0 then (FreeSpace(remainingSpace), queue) else (queue.head, queue.tail)

    (FileChunk(file.id, filled), newHead, newFiller, newQueue, newSupply)
  }

  def frag(layout: Seq[DiskChunk]): Seq[FileChunk] = {
    val reversed = layout.reverse.collect { case a: FileChunk => a }

    // the state is currentChunk,currentFiller,remainingChuncks,remainingFiller
    val compressed = List.unfold((layout.head, reversed.head, layout.tail, reversed.tail)) {
      case (currentHead, currentFiller, queue, supply) =>
        currentHead match {
          case End => None
          case s: FreeSpace =>
            val (f, newHead, newFiller, newQueue, newSupply) = fillGap(s, currentFiller, queue, supply)
            Some(f, (newHead, newFiller, newQueue, newSupply))
          case f: FileChunk if f.id < currentFiller.id => Some(f, (queue.head, currentFiller, queue.tail, supply))
          case f: FileChunk                            => Some(currentFiller, (End, f, queue, supply))
        }
    }
    compressed
  }

  def viz(layout: Seq[DiskChunk]): String = {
    layout.map {
      case f: FileChunk => f.id.toString.repeat(f.length)
      case s: FreeSpace => ".".repeat(s.length)
    }.mkString
  }

  def checksum(input: Seq[DiskChunk]): Long = {
    val (result, _) = input.foldLeft((0L, 0)) { case ((acc, inx), chunk) =>
      chunk match {
        case FreeSpace(len) => (acc, inx + len)
        case End            => ???
        case FileChunk(id, len) =>
          val sum = (inx until inx + len).foldLeft(0L) { case (ac2, ix2) => ac2 + (id * ix2) }
          (acc + sum, inx + len)
      }
    }
    result
  }

  @main
  def Day9main(): Unit = {
    val testIn     = parse(os.read(os.pwd / "input" / "Day9test.txt"))
    val testResult = frag(testIn)
    assert(viz(testResult) == "0099811188827773336446555566")
    val chk = checksum(testResult)
    assert(chk == 1928)

    val input         = parse(os.read(os.pwd / "input" / "Day9.txt"))
    val partAResult   = frag(input)
    val partAChecksum = checksum(partAResult)
    println(s"part A : $partAChecksum")
  }

}
