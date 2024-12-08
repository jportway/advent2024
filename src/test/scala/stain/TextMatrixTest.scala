package stain

import munit.FunSuite

class TextMatrixTest extends FunSuite {

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

}
