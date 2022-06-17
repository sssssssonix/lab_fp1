import scala.collection.immutable.Stream.Empty
import munit.EmptyPrinter

class MySuite extends munit.FunSuite {
  test("join test1") {
    val obtained = Set.join(Set.makeSet(), "", "", "")
    val expected = ""
    assertEquals(obtained, expected)
  }

  test("join test2") {
    val obtained = Set.join(Set.makeSet(), "[", ", ", "]")
    val expected = "[]"
    assertEquals(obtained, expected)
  }
  
  test("join test3") {
    val obtained = Set.join(Set.makeSet[Int](1), "[", ", ", "]")
    val expected = "[1]"
    assertEquals(obtained, expected)
  }

  test("join test4") {
    val obtained = Set.join(Set.makeSet[Int](1,2), "[", ", ", "]")
    val expected = "[1, 2]"
    assertEquals(obtained, expected)
  }

  test("join test5") {
    val obtained = Set.join(Set.makeSet[Int](1,2,2,3), "[", ", ", "]")
    val expected = "[1, 2, 3]"
    assertEquals(obtained, expected)
  }

  test("xs is subset of ys") {
    val obtained = Set.isSubsetOf(Set.makeSet[Int](2,3,5), Set.makeSet[Int](1,2,3,4,5,6))
    val expected = true
    assertEquals(obtained, expected)
  }

  test("xs(empty set) is subset of ys") {
    val obtained = Set.isSubsetOf(Set.makeSet[Int](), Set.makeSet[Int](1,2,3,4,5,6))
    val expected = true
    assertEquals(obtained, expected)
  }

  test("disjoint") {
    val obtained = Set.disjoint(Set.makeSet[Int](1,2,3), Set.makeSet[Int](4,5,6))
    val expected = true
    assertEquals(obtained, expected)
  }

  test("disjoint test") {
    val obtained = Set.disjoint(Set.makeSet[Int](1,2,3,4), Set.makeSet[Int](4,5,6))
    val expected = false
    assertEquals(obtained, expected)
  }
}