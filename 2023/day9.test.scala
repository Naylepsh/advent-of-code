class Day9Tests extends munit.FunSuite:
  import day9.*

  test("card extrapolation"):
    val input  = List(0, 3, 6, 9, 12, 15)
    val ds = prepareForExtrapolation(input)
    val result = extrapolate(ds)
    assert(result == 18)
