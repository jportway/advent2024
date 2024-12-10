import os.*

object Day9 {

  type ID = Int

  trait DiskChunk {

    def getId: Option[ID]
    def span: Int
    def isFile: Boolean
    def isFree: Boolean
    def shrink(l: Int): Option[DiskChunk]

  }

  case class FileChunk(id: ID, span: Int) extends DiskChunk {

    def isFile: Boolean = true
    def isFree: Boolean = false
    def sameFileAs(other: DiskChunk): Boolean = {
      other match {
        case o: FileChunk => o.id == this.id
        case _            => false
      }
    }
    override def getId: Option[ID]                 = Some(id)
    override def shrink(l: Int): Option[FileChunk] = Option.when(l < span)(this.copy(span = span - l))

  }

  case class FreeSpace(span: Int) extends DiskChunk {

    def getId                             = None
    def isFile: Boolean                   = false
    def isFree: Boolean                   = true
    def shrink(l: Int): Option[FreeSpace] = Option.when(l < span)(FreeSpace(span - l))

  }

  def parse(in: String): List[DiskChunk] = {
    val inStream = (in + "0").grouped(2).zipWithIndex.toList
    val mappedStream =
      inStream.flatMap((str, id) => List(FileChunk(id, str(0) - '0'), FreeSpace(str(1) - '0')))
    val layout = mappedStream.filter(_.span > 0) // remove zero length gaps
    layout
  }

  def frag(layout: List[DiskChunk]): List[DiskChunk] = {
    val backwards = layout.reverse.collect { case a: FileChunk => a }
    List.unfold(layout, backwards) { case (remainder: List[DiskChunk], supply: List[FileChunk]) =>
      remainder match {
        case Nil => None // nothing left to work with
        case next :: pending =>
          next match {
            case f: FileChunk => Some((f, (pending, supply)))
            case spc: FreeSpace =>
              val spanToFill = spc.span
              val filler     = supply.head
              val filledSpan = Math.min(spanToFill, filler.span)
              val fill       = filler.copy(span = filledSpan)
              val newSupply  = filler.shrink(filledSpan).toList ++ supply.tail
              val newRemainder = if (newSupply.head.sameFileAs(pending.head)) { // we're done
                List(newSupply.head) // just have to copy any remaining from the current file
              } else {
                spc.shrink(filledSpan).toList ++ pending
              }
              Some((fill, (newRemainder, newSupply)))
          }
      }
    }
  }

  def defrag(layout: List[DiskChunk]): List[DiskChunk] = {
    val maxId = layout.map(_.getId.getOrElse(0)).max
    (maxId to 0 by -1)
      .foldLeft(layout) { case (acc: List[DiskChunk], id) =>
        val (beforeMover, afterMover)   = acc.span(!_.getId.contains(id))
        val mover                       = afterMover.head
        val remnants                    = afterMover.tail
        val (beforeInsert, afterInsert) = beforeMover.span(x => x.isFile | x.span < mover.span)
        afterInsert match {
          case List() => acc // haven't found anywhere to move it so leave it where it is
          case space :: afterSpace =>
            val shrunk    = space.shrink(mover.span).toList // empty list if the space has disappeared otherwise a smaller space
            val spaceLeft = FreeSpace(mover.span)           // the gap where the mover used to be
            List.concat(beforeInsert, mover :: shrunk, afterSpace, spaceLeft :: remnants)
        }
      }
  }

  /** find the first free space that can fit the given size */
  def findFirstFree(layout: Seq[DiskChunk], len: Int): Unit = {
    val pos             = layout.indexWhere(x => x.isInstanceOf[FreeSpace] && x.span >= len)
    val (before, after) = layout.splitAt(pos)

  }

  /** produces a lazily computed sequence of blocks with the id of each one */
  def blocks(input: Seq[DiskChunk]): Iterable[Option[ID]] =
    input.view.flatMap(chunk => Seq.fill(chunk.span)(chunk.getId))

  def checksum(input: Seq[DiskChunk]): Long = blocks(input).zipWithIndex.foldLeft(0L) { case (acc, (n, idx)) =>
    acc + (n.getOrElse(0) * idx)
  }

  def viz(layout: Seq[DiskChunk]): String = {
    blocks(layout).map {
      case None     => "."
      case Some(id) => id.toString
    }.mkString
  }

  @main
  def Day9main(): Unit = {
    val input         = parse(os.read(os.pwd / "input" / "Day9.txt"))
    val partAResult   = frag(input)
    val partAChecksum = checksum(partAResult)
    println(s"part A : $partAChecksum")

    val partBResult   = defrag(input)
    val partBChecksum = checksum(partBResult)
    println(s"part B : $partBChecksum")
  }

  @main
  def day9test(): Unit = {
    val testIn = parse(os.read(os.pwd / "input" / "Day9test.txt"))
    println(testIn)
    println(viz(testIn))
    val testResult = frag(testIn)
    println(viz(testResult))
    assert(viz(testResult) == "0099811188827773336446555566")
    val chk = checksum(testResult)
    assert(chk == 1928)

    val test2 = defrag(testIn)
    println("\n" + viz(test2))
    assert(viz(test2) == "00992111777.44.333....5555.6666.....8888..")
    assert(checksum(test2) == 2858)
  }

}
