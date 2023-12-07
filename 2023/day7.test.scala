class Day7Tests extends munit.FunSuite:
  import day7.*
  import Card.given

  test("card comparison"):
    assert(Ordering[Card].gt(Card.`3`, Card.`2`))

  test("hands comparison"):
    val input =
      List("32T3K", "T55J5", "KK677", "KTJJT", "QQQJA").map(Hand.of)
    val expected =
      List("32T3K", "KTJJT", "KK677", "T55J5", "QQQJA").map(Hand.of)

    val actual = input.sorted
    assert(actual == expected)
