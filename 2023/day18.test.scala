class Day18Tests extends munit.FunSuite:
  import day18.*

  test("shoelace"):
    val points =
      List(Point(1, 6), Point(3, 1), Point(7, 2), Point(4, 4), Point(8, 5))
    assert(Point.shoelace(points) == 16)
