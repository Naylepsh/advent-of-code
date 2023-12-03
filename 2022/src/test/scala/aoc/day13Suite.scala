package aoc

import io.circe._
import io.circe.parser._
import day13._

class day13Suite extends munit.FunSuite:
  import day13Suite._

  test("parsing simple list") {
    val result = execute("[1,1,3,1,1]", "[1,1,5,1,1]")

    assertEquals(result, Comparison.Lower)
  }

  test("parsing nested lists") {
    val result = execute("[[1],[2,3,4]]", "[[1],4]")

    assertEquals(result, Comparison.Lower)
  }

  test("parsing simple list and nested one") {
    val result = execute("[9]", "[[8,7,6]]")

    assertEquals(result, Comparison.Greater)
  }

  test("parsing right list being longer") {
    val result = execute("[[4,4],4,4]", "[[4,4],4,4,4]")

    assertEquals(result, Comparison.Lower)
  }

  test("parsing left list being longer") {
    val result = execute("[7,7,7,7]", "[7,7,7]")

    assertEquals(result, Comparison.Greater)
  }

  test("parsing empty list") {
    val result = execute("[]", "[3]")

    assertEquals(result, Comparison.Lower)
  }

  test("parsing nested empty lists") {
    val result = execute("[[[]]]", "[[]]")

    assertEquals(result, Comparison.Greater)
  }

  test("parsing deeply nested lists") {
    val result =
      execute("[1,[2,[3,[4,[5,6,7]]]],8,9]", "[1,[2,[3,[4,[5,6,0]]]],8,9]")

    assertEquals(result, Comparison.Greater)
  }

  test("pair 33") {
    val result =
      execute(
        "[[[],3,[[2,2,7,4,7],1]],[6],[[1,[9]],[[3,6,10,2,0],[10,3,0,6,1],8,7],[[3,7,5,9],5,1]],[0]]",
        "[[[]],[],[],[1,1,6,[[8,0,5,9,10],6],8]]"
      )

    assertEquals(result, Comparison.Greater)

  }

  test("pair 66") {
    val result =
      execute(
        "[[5,[[0],6,[7,8,7,5],[4,8,7,7],10],[0,9,[4,9,9,6,3],[6],4],[[8,9],3,[]]],[[]],[0,[[10,10,5,8,5],2,7,0,[3]],[2,[4,6,5,1,6],[7,10,10,4],7]]]",
        "[[5,0],[[[6,3,5],[3],[8,1,5],5,9],[6],[2,0],2,[10]],[]]"
      )

    assertEquals(result, Comparison.Greater)

  }


object day13Suite:
  def execute(a: String, b: String): Comparison =
    (parse(a), parse(b)) match
      case (Right(left), Right(right)) =>
        // println("=" * 30)
        // println(s"$left :: $right")
        // println("=" * 30)
        compare(left, right)
