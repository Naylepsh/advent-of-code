class Day9Tests extends munit.FunSuite:
  import day9.*

  test("extrapolation for p1"):
    val input  = List(0, 3, 6, 9, 12, 15)
    val ds     = prepareForExtrapolation(input)
    val result = extrapolateP1(ds)
    assert(result == 18)

  test("extrapolation for p2 (history 1)"):
    val input  = List(0, 3, 6, 9, 12, 15)
    val ds     = prepareForExtrapolation(input)
    val result = extrapolateP2(ds)
    assert(result == -3)

  test("extrapolation for p2 (history 2)"):
    val input  = List(1, 3, 6, 10, 15, 21)
    val ds     = prepareForExtrapolation(input)
    val result = extrapolateP2(ds)
    assert(result == 0)

  test("extrapolation for p2 (history 3)"):
    val input  = List(10, 13, 16, 21, 30, 45)
    val ds     = prepareForExtrapolation(input)
    val result = extrapolateP2(ds)
    assert(result == 5)

  test("extrapolation for p2 (custom input)"):
    val input = List(
      17, 21, 20, 14, 13, 42, 150, 428, 1051, 2390, 5312, 11943, 27490, 64328,
      150639, 347692, 782705, 1708551, 3607880, 7368160, 14566447
    )
    val ds     = prepareForExtrapolation(input)
    val result = extrapolateP2(ds)
    println(s"result: $result")
    assert(result == 8)
