import stain.Direction
import stain.Position
import stain.TextMatrix

object Day10 {

  trait Path {

    def isLeaf: Boolean
    def isBranch: Boolean
    def hasLeaf: Boolean

  }
  case class Leaf(position: TextMatrix#ValidCell) extends Path {

    override def isLeaf: Boolean   = true
    override def isBranch: Boolean = false
    override def hasLeaf: Boolean  = true

  }
  case class Branch(position: TextMatrix#ValidCell, children: List[Path]) extends Path {

    override def isBranch: Boolean = true
    override def isLeaf: Boolean   = false
    override def hasLeaf: Boolean  = children.exists(_.hasLeaf)

  }

  def loadMap(path: os.Path): TextMatrix = {
    val str    = os.read.lines(path)
    val matrix = TextMatrix(str)
    matrix
  }

  def findPaths(mat: TextMatrix, pnt: mat.ValidCell): Path = {
    val height = pnt.value
    if (height == '9') {
      Leaf(pnt)
    } else {
      val nextHeight                   = height + (1.toChar)
      val neighbours: Vector[mat.Cell] = pnt.cardinalNeighbours
      val validNeighbours = neighbours.collect {
        case x: mat.ValidCell if x.value == nextHeight => x
      }
      val paths = validNeighbours.toList.map(x => findPaths(mat, x)).filter(_.hasLeaf)
      Branch(pnt, paths)
    }
  }

  def findEndpoints(path: Path): List[TextMatrix#ValidCell] =
    path match {
      case l: Leaf   => List(l.position)
      case b: Branch => b.children.flatMap(findEndpoints)
    }

  def score(paths: Seq[Path]): Int = paths.map(x => findEndpoints(x).distinct.length).sum

  def findAllPaths(mat: TextMatrix): Seq[Path] = {
    val startPoints = mat.find(_ == '0')
    startPoints.map(findPaths(mat, _))
  }

  @main
  def Day10main(): Unit =
    val paths = findAllPaths(loadMap(os.pwd / "input" / "day10test.txt"))
    assert(score(paths) == 36)

    val pathsA = findAllPaths(loadMap(os.pwd / "input" / "day10.txt"))
    println(s"part A : ${score(pathsA)}")

}
