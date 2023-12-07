import scala.util.chaining.*

object day7p2:
  enum Card:
    case J, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, T, Q, K, A
  object Card:
    given Ordering[Card] with
      def compare(a: Card, b: Card): Int = a.ordinal compare b.ordinal

  enum HandType:
    case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind,
      FiveOfAKind

  object HandType:
    private def ofInternal(hand: List[Card]): HandType =
      val cardCount = hand.groupBy(identity).map(_._2.length).toList.sorted

      cardCount match
        case 5 :: Nil                => FiveOfAKind
        case 1 :: 4 :: Nil           => FourOfAKind
        case 2 :: 3 :: Nil           => FullHouse
        case 1 :: 1 :: 3 :: Nil      => ThreeOfAKind
        case 1 :: 2 :: 2 :: Nil      => TwoPair
        case 1 :: 1 :: 1 :: 2 :: Nil => OnePair
        case _                       => HighCard

    def of(hand: List[Card]): HandType =
      val candidates = Card.values.filterNot(_ == Card.J)

      def aux(jokers: Int, rest: List[Card]): HandType =
        jokers match
          case 0 => HandType.ofInternal(rest)
          case n => candidates
              .map: candidate =>
                aux(n - 1, candidate :: rest)
              .max

      aux(hand.filter(_ == Card.J).length, hand.filterNot(_ == Card.J))

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
      val diff = Ordering[HandType].compare(HandType.of(as), HandType.of(bs))
      if diff != 0 then diff
      else
        // Compare on first card matchup that differs
        as.zip(bs).find(_ != _).map(Ordering[Card].compare(_, _)).getOrElse(0)

  def solve(hands: List[(List[Card], Int)]): Int =
    hands
      .sortBy(_._1)
      .zipWithIndex
      .foldLeft(0): (acc, current) =>
        val ((_, bid), idx) = current
        acc + bid * (idx + 1)

@main def runDay7p2 =
  import day7p2.*

  val lines = scala.io.Source.fromFile("./day7.input").getLines.toList

  lines.map(parse).pipe(solve.andThen(println))
