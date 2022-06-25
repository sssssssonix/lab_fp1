import scala.collection.immutable.Stream.Empty
import munit.EmptyPrinter

class MySuite extends munit.FunSuite {
  test("count of element test1") {
    val obtained = Set.getCountOfElement(Set.makeSet[Int](1,2,5,5,6,5,5,6,9), 5)
    val expected = 4
    assertEquals(obtained, expected)
  }
  
  test("count of element test2") {
    val obtained = Set.getCountOfElement(Set.makeSet[Int](), 1)
    val expected = 0
    assertEquals(obtained, expected)
  }

  test("most common element test1") {
    val obtained = Set.mostCommonElement(Set.makeSet[Int](1,2,5,6,5,5,6,9))
    val expected = Some(5)
    assertEquals(obtained, expected)
  }

  test("most common element test2") {
    val obtained = Set.mostCommonElement(Set.makeSet[Int]())
    val expected = None
    assertEquals(obtained, expected)
  }
  
  test("union test1") {
    val obtained = Set.toString(Set.union(Set.makeSet[Int](1,2,3), Set.makeSet[Int](4,5,6)))
    val expected = "6x1 5x1 4x1 1x1 2x1 3x1"
    assertEquals(obtained, expected)
  }

  test("union test2") {
    val obtained = Set.toString(Set.union(Set.makeSet[Int](1,2,3,3,6), Set.makeSet[Int](4,5,6,2)))
    val expected = "5x1 4x1 1x1 2x2 6x2 3x2"
    assertEquals(obtained, expected)
  }

  test("difference test1") {
    val obtained = Set.toString(Set.difference(Set.makeSet[Int](1,2,5,6,10), Set.makeSet[Int](4,5,6,11)))
    val expected = "10x1 2x1 1x1"
    assertEquals(obtained, expected)
  }

  test("difference test2") {
    val obtained = Set.toString(Set.difference(Set.makeSet[Int](5,5,6,5), Set.makeSet[Int]()))
    val expected = "5x3 6x1"
    assertEquals(obtained, expected)
  }

  test("intersection test1") {
    val obtained = Set.toString(Set.intersection(Set.makeSet[Int](1,1,6,10,4,6,13), Set.makeSet[Int](1,4,5,10)))
    val expected = "10x1 4x1 1x1"
    assertEquals(obtained, expected)
  }

  test("intersection test2") {
    val obtained = Set.toString(Set.intersection(Set.makeSet[Int](1,1,6,10,4), Set.makeSet[Int]()))
    val expected = ""
    assertEquals(obtained, expected)
  }
}
