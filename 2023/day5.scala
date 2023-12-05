import scala.annotation.tailrec

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
    ThingMap.map(seedToSoil)
      .andThen(ThingMap.map(soilToFertilizer))
      .andThen(ThingMap.map(fertilizerToWater))
      .andThen(ThingMap.map(waterToLight))
      .andThen(ThingMap.map(lightToTemperature))
      .andThen(ThingMap.map(temperatureToHumidity))
      .andThen(ThingMap.map(humidityToLocation))(a)

  def emap(a: BigInt): BigInt =
    ThingMap.emap(humidityToLocation)
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
    if n % 1_000_000 == 0 then 
      println(s"$n / ???")
    smallestLocationInRange(seedRanges, maps, n + 1)

@main def run: Unit =
  val lines         = scala.io.Source.fromFile("./day5.input").getLines.toList
  val (seeds, maps) = Parser.parse(lines)

  // part 1
  println(smallestLocation(seeds, maps))

  // part 2
  val ranges = SeedRanges.fromSeeds(seeds)
  println(smallestLocationInRange(ranges, maps))
