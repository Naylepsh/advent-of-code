//> using dep "org.typelevel::cats-core:2.10.0"

import scala.annotation.tailrec
import cats.syntax.all.*

object V2:
  case class Range(from: BigInt, to: BigInt)
  object Range:
    def apply(from: BigInt, to: BigInt): Range =
      new Range(from.min(to), from.max(to))

    def apply(range: Range, rangeMap: RangeMap): Option[(Range, List[Range])] =
      val other = rangeMap.asRange

      val result =
        if other.to < range.from then
          //        [---]
          //  [---]
          None
        else if other.from <= range.from && range.from < other.to then
          //   [----]
          // [-------]
          (
            Some(
              // the common subset with mapping applied
              Range(range.from + rangeMap.offset, range.to + rangeMap.offset),
              List.empty
            ),
        )
        else if other.from <= range.from && other.to <= range.from then
          //    [----]
          // [----]
          // the common subset with mapping applied
          Some(
            Range(range.from + rangeMap.offset, other.to + rangeMap.offset),
            // the right of range without mapping,
            List(Range(other.to, range.to))
          )
        else if range.from <= other.from && other.to <= range.to then
          // [-------]
          //  [----]
          //
          Some(
            // the common subset with mapping applied
            Range(other.from + rangeMap.offset, other.to + rangeMap.offset),
            List(
              // the left of range without mapping
              Range(range.from, other.from),
              // the left of range without mapping
              Range(other.to, range.to)
            )
          )
        else if other.from <= range.to && range.to <= other.to then
          // [----]
          //   [----]
          // the common subset with mapping applied
          Some(
            Range(other.from + rangeMap.offset, range.to + rangeMap.offset),
            List(
              // the left of range without mapping
              Range(range.from, other.from)
            )
          )
        else
          //  [---]
          //        [---]
          None
      println(s"[apply-single]($range, $rangeMap) = $result")
      result

    def apply(range: Range, rangeMaps: List[RangeMap]): List[Range] =
      // Apply all range maps to the range
      var applied = List.empty[Range]
      var leftover = List.empty[Range]
      rangeMaps
        .foreach: map =>
          Range.apply(range, map) match
            case None =>
            case Some(matched, leftoverParts) =>
              applied = matched :: leftoverParts
              leftover = leftover ::: leftoverParts
      if applied.isEmpty then applied = range :: Nil

      // TODO: Need to handle leftovers, but what to do with them?
      println(s"[apply-many]($range, $rangeMaps) = $applied and $leftover")
      applied 

    def apply(
        ranges: List[Range],
        rangeMapss: List[List[RangeMap]]
    ): List[Range] =
      // Keep applying continous range maps to all ranges until run out of range maps
      rangeMapss match
        case Nil => ranges
        case head :: tail =>
          Range.apply(ranges.flatMap(Range.apply(_, head)), tail)

    def earliest(ranges: List[Range]): Option[Range] =
      ranges.sortBy(_.from).headOption

  case class RangeMap(from: BigInt, to: BigInt, offset: BigInt):
    def asRange: Range = Range(from, to)

  object RangeMap:
    def parse(line: String): RangeMap =
      val Array(destination, source, range) =
        line.split(" ").map(BigInt(_)): @unchecked
      val offset = destination - source

      RangeMap(source, source + range, offset)

    def parse(lines: List[String]): (List[RangeMap], List[String]) =
      parse(lines, List.empty)

    @tailrec
    def parse(
        lines: List[String],
        acc: List[RangeMap]
    ): (List[RangeMap], List[String]) =
      lines match
        case Nil          => (acc.reverse, Nil)
        case "" :: tail   => (acc.reverse, tail)
        case head :: tail => parse(tail, RangeMap.parse(head) :: acc)

  object Parser:
    def parse(lines: List[String]): (List[Range], List[List[RangeMap]]) =
      val ranges =
        lines.head
          .split("seeds: ")
          .tail
          .head
          .split(" ")
          .map(BigInt(_))
          .toList
          .grouped(2)
          .map:
            case from :: range :: Nil =>
              Range(from, from + range)
          .toList

      @tailrec
      def aux(
          lines: List[String],
          maps: List[List[RangeMap]]
      ): List[List[RangeMap]] =
        lines match
          case Nil => maps.reverse
          case lines =>
            val (map, leftoverLines) = RangeMap.parse(lines.tail)
            aux(leftoverLines, map :: maps)

      (ranges, aux(lines.tail.tail, List.empty))

  def solve2(file: String): Unit =
    val lines = scala.io.Source.fromFile(file).getLines.toList
    val (ranges, rangeMaps) = Parser.parse(lines)

    Range.earliest(Range.apply(ranges, rangeMaps)).foreach(println)
case class ThingRange(
    sourceStart: BigInt,
    destinationStart: BigInt,
    range: BigInt
)
object ThingRange:
  def parse(line: String): ThingRange =
    val Array(destinationStart, sourceStart, range) =
      line.split(" ").map(BigInt(_)): @unchecked

    ThingRange(sourceStart, destinationStart, range)

type ThingMap = List[ThingRange]
object ThingMap:
  def parse(lines: List[String]): (ThingMap, List[String]) =
    parse(lines.tail, List.empty)

  @tailrec
  def parse(lines: List[String], acc: ThingMap): (ThingMap, List[String]) =
    lines match
      case Nil          => (acc.reverse, Nil)
      case "" :: tail   => (acc.reverse, tail)
      case head :: tail => parse(tail, ThingRange.parse(head) :: acc)

  def map(ranges: ThingMap)(a: BigInt): BigInt =
    ranges
      .find: r =>
        r.sourceStart <= a && a < r.sourceStart + r.range
      .map: r =>
        val offset = a - r.sourceStart
        r.destinationStart + offset
      .getOrElse(a)

  def emap(ranges: ThingMap)(a: BigInt): BigInt =
    ranges
      .find: r =>
        r.destinationStart <= a && a < r.destinationStart + r.range
      .map: r =>
        val offset = a - r.destinationStart
        r.sourceStart + offset
      .getOrElse(a)

case class Maps(
    seedToSoil: ThingMap,
    soilToFertilizer: ThingMap,
    fertilizerToWater: ThingMap,
    waterToLight: ThingMap,
    lightToTemperature: ThingMap,
    temperatureToHumidity: ThingMap,
    humidityToLocation: ThingMap
):
  def map(a: BigInt): BigInt =
    ThingMap
      .map(seedToSoil)
      .andThen(ThingMap.map(soilToFertilizer))
      .andThen(ThingMap.map(fertilizerToWater))
      .andThen(ThingMap.map(waterToLight))
      .andThen(ThingMap.map(lightToTemperature))
      .andThen(ThingMap.map(temperatureToHumidity))
      .andThen(ThingMap.map(humidityToLocation))(a)

  def emap(a: BigInt): BigInt =
    ThingMap
      .emap(humidityToLocation)
      .andThen(ThingMap.emap(temperatureToHumidity))
      .andThen(ThingMap.emap(lightToTemperature))
      .andThen(ThingMap.emap(waterToLight))
      .andThen(ThingMap.emap(fertilizerToWater))
      .andThen(ThingMap.emap(soilToFertilizer))
      .andThen(ThingMap.emap(seedToSoil))(a)

type Seeds = List[BigInt]

object Parser:
  def parse(lines: List[String]): (Seeds, Maps) =
    val seeds =
      lines.head.split("seeds: ").tail.head.split(" ").map(BigInt(_)).toList

    @tailrec
    def aux(lines: List[String], maps: List[ThingMap]): List[ThingMap] =
      lines match
        case Nil => maps
        case lines =>
          val (map, leftoverLines) = ThingMap.parse(lines)
          aux(leftoverLines, map :: maps)

    val maps = aux(lines.tail.tail, List.empty)
    (
      seeds,
      maps match
        case humidityToLocation :: temperatureToHumidity :: lightToTemperature :: waterToLight :: fertilizerToWater :: soilToFertilizer :: seedToSoil :: Nil =>
          Maps(
            seedToSoil,
            soilToFertilizer,
            fertilizerToWater,
            waterToLight,
            lightToTemperature,
            temperatureToHumidity,
            humidityToLocation
          )
    )

def smallestLocation(seeds: Seeds, maps: Maps): BigInt =
  seeds
    .map: seed =>
      maps.map(seed)
    .min

type SeedRanges = List[(BigInt, BigInt)]
object SeedRanges:
  def fromSeeds(seeds: Seeds): SeedRanges =
    seeds
      .grouped(2)
      .map:
        case start :: range :: Nil => start -> range
      .toList

  def contains(ranges: SeedRanges)(a: BigInt): Boolean =
    @tailrec
    def aux(ranges: SeedRanges): Boolean =
      ranges match
        case Nil => false
        case (start, range) :: tail =>
          if start <= a && a <= start + range then true else aux(tail)

    aux(ranges)

def smallestLocationInRange(seedRanges: SeedRanges, maps: Maps): BigInt =
  smallestLocationInRange(seedRanges, maps, 0)
def smallestLocationInRange(
    seedRanges: SeedRanges,
    maps: Maps,
    n: BigInt
): BigInt =
  // Low-effort bruteforce,
  // Garbage tier performance,
  // You love to see it
  if SeedRanges.contains(seedRanges)(maps.emap(n)) then n
  else
    if n % 1_000_000 == 0 then println(s"$n / ???")
    smallestLocationInRange(seedRanges, maps, n + 1)

@main def run: Unit =
  // val lines = scala.io.Source.fromFile("./day5.input").getLines.toList
  // val (seeds, maps) = Parser.parse(lines)
  //
  // // part 1
  // println(smallestLocation(seeds, maps))
  //
  // // part 2
  // val ranges = SeedRanges.fromSeeds(seeds)
  // println(smallestLocationInRange(ranges, maps))
  V2.solve2("./day5.input")
