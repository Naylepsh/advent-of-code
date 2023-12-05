//> using test.dep org.scalameta::munit::0.7.29
//> using option -Yrangepos

class Day5Tests extends munit.FunSuite:
  test("ThingMap parsing"):
    val input = """seed-to-soil map:
      |50 98 2
      |52 50 48

      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15"""".stripMargin.split("\n").toList
    val expectedMap = List(
      ThingRange(98, 50, 2),
      ThingRange(50, 52, 48)
    )
    val expectedLines = """soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15"""".stripMargin.split("\n").toList

    val (map, lines) = ThingMap.parse(input)

    assert(map == expectedMap)
    assert(lines == expectedLines)

  test("map parsing"):
    val input = """seeds: 79 14 55 13

      |seed-to-soil map:
      |50 98 2
      |52 50 48

      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15

      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4

      |water-to-light map:
      |88 18 7
      |18 25 70

      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13

      |temperature-to-humidity map:
      |0 69 1
      |1 0 69

      |humidity-to-location map:
      |60 56 37
      |56 93 4""".stripMargin.split("\n").toList
    val expectedSeeds = List(79, 14, 55, 13)
    val expectedMaps = Maps(
      seedToSoil = List(
        ThingRange(98, 50, 2),
        ThingRange(50, 52, 48)
      ),
      soilToFertilizer = List(
        ThingRange(15, 0, 37),
        ThingRange(52, 37, 2),
        ThingRange(0, 39, 15)
      ),
      fertilizerToWater = List(
        ThingRange(53, 49, 8),
        ThingRange(11, 0, 42),
        ThingRange(0, 42, 7),
        ThingRange(7, 57, 4)
      ),
      waterToLight = List(
        ThingRange(18, 88, 7),
        ThingRange(25, 18, 70)
      ),
      lightToTemperature = List(
        ThingRange(77, 45, 23),
        ThingRange(45, 81, 19),
        ThingRange(64, 68, 13)
      ),
      temperatureToHumidity = List(
        ThingRange(69, 0, 1),
        ThingRange(0, 1, 69)
      ),
      humidityToLocation = List(
        ThingRange(56, 60, 37),
        ThingRange(93, 56, 4)
      )
    )

    val (seeds, maps) = Parser.parse(input)

    assert(seeds == expectedSeeds)
    assert(maps == expectedMaps)

  test("mapping in range"):
    val map = List(
      ThingRange(98, 50, 2),
      ThingRange(50, 52, 48)
    )
    val res = ThingMap.map(map)(79)
    assert(res == 81)

  test("mapping outside of range"):
    val map = List(
      ThingRange(98, 50, 2),
      ThingRange(50, 52, 48)
    )
    val res = ThingMap.map(map)(13)
    assert(res == 13)


  test("emapping in range"):
    val map = List(
      ThingRange(98, 50, 2),
      ThingRange(50, 52, 48)
    )
    val res = ThingMap.emap(map)(81)
    assert(res == 79)


  test("emapping entire map"):
    val maps = Maps(
      seedToSoil = List(
        ThingRange(98, 50, 2),
        ThingRange(50, 52, 48)
      ),
      soilToFertilizer = List(
        ThingRange(15, 0, 37),
        ThingRange(52, 37, 2),
        ThingRange(0, 39, 15)
      ),
      fertilizerToWater = List(
        ThingRange(53, 49, 8),
        ThingRange(11, 0, 42),
        ThingRange(0, 42, 7),
        ThingRange(7, 57, 4)
      ),
      waterToLight = List(
        ThingRange(18, 88, 7),
        ThingRange(25, 18, 70)
      ),
      lightToTemperature = List(
        ThingRange(77, 45, 23),
        ThingRange(45, 81, 19),
        ThingRange(64, 68, 13)
      ),
      temperatureToHumidity = List(
        ThingRange(69, 0, 1),
        ThingRange(0, 1, 69)
      ),
      humidityToLocation = List(
        ThingRange(56, 60, 37),
        ThingRange(93, 56, 4)
      )
    )
    val res = maps.emap(46)
    assert(res == 82)

