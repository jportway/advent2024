package stain

import munit.FunSuite

class SpacialMatrixTest extends FunSuite {

  test("apply location") {
    val tmA = TextMatrix(Vector("abc", "def", "ghk"))
    val tmB = TextMatrix(Vector("123", "456", "789"))

    val loc1    = tmA(1, 2)
    val another = tmA(1, 2)
    assert(!(loc1 eq another)) // two different location objects
    val loc3 = tmA(loc1)
    assert(loc1 eq loc3) // if i pass in a location object and it alredy comes from this TM then i should just get the same one back
    assertEquals(loc1.content.get, 'h')

    val loc2 = tmB(loc1)
    assert(!(loc1 eq loc2)) // if I look at the same position in a different matrix i should get a differeent location back
    assertEquals(loc2.content.get, '8')

  }

  test("scanRowLeft") {
    val tmA = TextMatrix(Vector("abc", "def", "ghi"))
    val transformed: SimpleMatrix[String] = tmA.scanMap { (cell, prev, above) =>
      cell.value.toString + (prev.getOrElse('.').toString.head) + (above.getOrElse('.').toString.head)
    }
    val expected =
      IndexedSeq(IndexedSeq("a..", "ba.", "cb."), IndexedSeq("d.a", "edb", "fec"), IndexedSeq("g.d", "hge", "ihf"))
    assertEquals(transformed.contents, expected)
  }

  test("connected") {
    val tm     = TextMatrix(Vector("XXO", "XOO", "XXO"))
    val result = tm.connected(tm.locations.head, Direction.cardinals)((cell, _, _) => cell.value == 'X')
    println(result.toList)
    val resultSet = result.toSet
    val expected  = Set(tm(0, 0), tm(1, 0), tm(0, 1), tm(0, 2), tm(1, 2)).map(_.toOption.get)
    assertEquals(resultSet, expected)

    val result2    = tm.connected(tm(2, 0).toOption.get, Direction.cardinals)((cell, _, _) => cell.value == 'O')
    val resultSet2 = result2.toSet
    val expected2  = Set(tm(2, 0), tm(1, 1), tm(2, 1), tm(2, 2)).map(_.toOption.get)
    assertEquals(resultSet2, expected2)

  }

  test("regions") {
    val tm = TextMatrix(Vector("XXO", "XOO", "XXO"))
    val result = tm.connectedRegions(Direction.cardinals) { (cell, fromCell, _) =>
      cell.value == fromCell.value
    }
    val resultSet = result.map(_.toSet).toSet
    println(result.toList)
    val reg1     = Set(tm(0, 0), tm(1, 0), tm(0, 1), tm(0, 2), tm(1, 2)).map(_.toOption.get)
    val reg2     = Set(tm(2, 0), tm(1, 1), tm(2, 1), tm(2, 2)).map(_.toOption.get)
    val expected = Set(reg1, reg2)
    assertEquals(resultSet, expected)
  }

}
