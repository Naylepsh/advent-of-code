class Day13Tests extends munit.FunSuite:
  import day13.*
  import Day13Tests.*

  test("find reflection index"):
    assert(findReflectionIndex(firstMirror.transpose) == Some(5))
    assert(findReflectionIndex(secondMirror) == Some(4))

  test("find smudged reflection index"):
    assert(findSmudgedReflectionIndex(firstMirror) == Some(3))
    assert(findSmudgedReflectionIndex(secondMirror) == Some(1))

object Day13Tests:
  val firstMirror =
    """#.##..##.
     |..#.##.#.
     |##......#
     |##......#
     |..#.##.#.
     |..##..##.
     |#.#.##.#.""".stripMargin.linesIterator.toList.map(_.toList)

  val secondMirror =
    """#...##..#
      |#....#..#
      |..##..###
      |#####.##.
      |#####.##.
      |..##..###
      |#....#..#""".stripMargin.linesIterator.toList.map(_.toList)
