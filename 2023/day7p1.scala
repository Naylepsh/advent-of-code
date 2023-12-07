import scala.util.chaining.*

object day7p1:
  enum Card:
    case `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, T, J, Q, K, A
  object Card:
    given Ordering[Card] with
      def compare(a: Card, b: Card): Int = a.ordinal compare b.ordinal

  enum HandType:
    case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind,
      FiveOfAKind

  object HandType:
    def of(hand: List[Card]): HandType =
      val cardCount = hand.groupBy(card => card).map(_._2.length).toList.sorted

      cardCount match
        case _ :: Nil                => FiveOfAKind
        case 1 :: 4 :: Nil           => FourOfAKind
        case 2 :: 3 :: Nil           => FullHouse
        case 1 :: 1 :: 3 :: Nil      => ThreeOfAKind
        case 1 :: 2 :: 2 :: Nil      => TwoPair
        case 1 :: 1 :: 1 :: 2 :: Nil => OnePair
        case _                       => HighCard

    given Ordering[HandType] with
      def compare(a: HandType, b: HandType): Int = a.ordinal compare b.ordinal

  object Hand:
    def of(str: String): List[Card] =
      str.map(c => Card.valueOf(c.toString)).toList

  def parse(line: String): (List[Card], Int) =
    val Array(rawCards, rawBid) = line.split(" "): @unchecked
    (Hand.of(rawCards), rawBid.toInt)

  given Ordering[List[Card]] with
    def compare(as: List[Card], bs: List[Card]): Int =
      val asHandType = HandType.of(as)
      val bsHandType = HandType.of(bs)

      val diff = Ordering[HandType].compare(asHandType, bsHandType)
      if diff != 0 then diff
      else
        as.zip(bs).find(_ != _).map(Ordering[Card].compare(_, _)).getOrElse(0)

  def solve(hands: List[(List[Card], Int)]): Int =
    hands
      .sortBy(_._1)
      .zipWithIndex
      .foldLeft(0): (acc, current) =>
        val ((_, bid), idx) = current
        acc + bid * (idx + 1)

@main def runDay7p1 =
  import day7p1.*

  val lines = scala.io.Source.fromFile("./day7.input").getLines.toList

  lines.map(parse).pipe(solve.andThen(println))
