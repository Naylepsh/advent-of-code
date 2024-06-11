package day12

import scala.annotation.tailrec
import scala.util.chaining.*

type Input = (String, List[Int])

def parse(data: List[String]): List[Input] =
  data.foldRight(List.empty): (line, acc) =>
    val Array(springs, rawGroupSizes) = line.split(" ")
    val groupSizes = rawGroupSizes.split(',').map(_.toInt).toList
    (springs, groupSizes) :: acc

class SpringCounter:
  private val cache =
    scala.collection.mutable.Map.empty[(List[Char], List[Int], Int), Long]

  def count(
      spring: List[Char],
      damagedSprings: List[Int],
      currentStreakLength: Int
  ): Long =
    cache.getOrElseUpdate(
      (spring, damagedSprings, currentStreakLength),
      countCacheless(spring, damagedSprings, currentStreakLength)
    )

  private def countCacheless(
      spring: List[Char],
      damagedSprings: List[Int],
      currentStreakLength: Int
  ) =
    spring match
      case Nil =>
        if damagedSprings.isEmpty && currentStreakLength == 0
        then 1L
        else if damagedSprings.head == currentStreakLength && damagedSprings.tail.isEmpty
        then 1L
        else 0L
      case head :: tail =>
        def handleOperationalSpring() =
          if currentStreakLength == 0 then count(tail, damagedSprings, 0)
          else if damagedSprings.headOption == Some(currentStreakLength) then
            count(tail, damagedSprings.tail, 0)
          else 0L
        def handleDamagedSpring() =
          if damagedSprings.isEmpty then 0L
          // The streak is longer than expected. Might as well cut short here
          else if damagedSprings.head == currentStreakLength then 0L
          else count(tail, damagedSprings, currentStreakLength + 1)

        head match
          case '#' => handleDamagedSpring()
          case '.' => handleOperationalSpring()
          case '?' => handleDamagedSpring() + handleOperationalSpring()

def part1(input: List[Input]): Long =
  val counter = new SpringCounter()
  input.foldLeft(0L):
    case (acc, (spring, damagedSprings)) =>
      acc + counter.count(spring.toList, damagedSprings, 0)

def part2(input: List[Input]): Long =
  val counter = new SpringCounter()
  input.foldLeft(0L):
    case (acc, (spring, damagedSprings)) =>
      val adjustedSpring = (0 until 5).map(_ => spring).mkString("?")
      val adjustedDamagedSprings =
        (0 until 5).flatMap(_ => damagedSprings).toList
      acc + counter.count(adjustedSpring.toList, adjustedDamagedSprings, 0)

@main
def run: Unit =
  scala.io.Source
    .fromFile("./day12.input")
    .getLines()
    .toList
    .pipe(parse)
    // .pipe(part1)
    .pipe(part2)
    .pipe(println)
