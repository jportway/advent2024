package stain

case class TextMatrix(contents: IndexedSeq[IndexedSeq[Char]]) extends SpacialMatrix[TextMatrix, Char] {

  override def copyWithContents(newContents: IndexedSeq[IndexedSeq[Char]]): TextMatrix =
    this.copy(contents = newContents.toVector)

  /** check for a string starting from a particular location in a particular direction */
  def stringCheck(searchString: String, currentPos: Position, direction: Direction): Boolean = {
    if searchString.isEmpty then true
    else {
      val matched = matchVal(currentPos, searchString.head)
      if matched then stringCheck(searchString.tail, currentPos + direction, direction)
      else false
    }
  }

}

object TextMatrix {

  type TextCell        = TextMatrix#CellBase[?]
  type ValidTextCell   = TextMatrix#ValidCell
  type InvalidTextCell = TextMatrix#InvalidCell

  def fromStrings(content: IndexedSeq[String]) = new TextMatrix(content.map(_.toVector))

}
