class Day11Tests extends munit.FunSuite:
  import day11.*
  import ExpandedSpaces.*
  import Galaxy.*
  import Day11Tests.*

  test("parsing"):
    val galaxies = Galaxies.parse(space)
    assert(galaxies.sorted == Day11Tests.galaxies.sorted)

  test("extended spaces"):
    val (rows, columns) = Galaxies.findExpandedSpaces(
      Day11Tests.galaxies,
      space.head.length - 1,
      space.length - 1
    )
    assert(rows == expandedSpaces.rows)
    assert(columns == expandedSpaces.columns)

  test("extending galaxy"):
    val extendedGalaxies = Galaxies.extend(galaxies, expandedSpaces)(using 1)
    assert(
      extendedGalaxies.sorted ==
        List(
          Galaxy(4, 0),
          Galaxy(9, 1),
          Galaxy(0, 2),
          Galaxy(8, 5),
          Galaxy(1, 6),
          Galaxy(12, 7),
          Galaxy(9, 10),
          Galaxy(0, 11),
          Galaxy(5, 11)
        )
    )

  test("distance"):
    assert(Galaxy(4, 0).distanceTo(Galaxy(9, 10)) == 15)
    assert(Galaxy(0, 2).distanceTo(Galaxy(12, 7)) == 17)
    assert(Galaxy(0, 11).distanceTo(Galaxy(5, 11)) == 5)

object Day11Tests:
  import day11.*

  val space =
    """...#......
      |.......#..
      |#.........
      |..........
      |......#...
      |.#........
      |.........#
      |..........
      |.......#..
      |#...#.....""".stripMargin.split('\n').toList

  val galaxies = List(
    Galaxy(3, 0),
    Galaxy(7, 1),
    Galaxy(0, 2),
    Galaxy(6, 4),
    Galaxy(1, 5),
    Galaxy(9, 6),
    Galaxy(7, 8),
    Galaxy(0, 9),
    Galaxy(4, 9)
  )

  val expandedSpaces: ExpandedSpaces = (List(3, 7), List(2, 5, 8))
