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

@main def run: Unit =
  val lines         = scala.io.Source.fromFile("./day5.input").getLines.toList
  val (seeds, maps) = Parser.parse(lines)
  println(smallestLocation(seeds, maps))
