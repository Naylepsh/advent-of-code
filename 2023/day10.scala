package day10

import scala.annotation.tailrec
import scala.collection.immutable.LazyList
import scala.util.chaining.*

type Position = (Int, Int)

type Field = Seq[String]

def findStart(field: Field): Position =
  val y = field.indexWhere(_.contains('S'))
  val x = field(y).indexOf('S')
  (x, y)

def connected(field: Field, position: Position): Set[Position] =
  // println(s"Checking connections for $position")
  val (x, y) = position
  field(y)(x) match
    case '|' => Set((x, y - 1), (x, y + 1))
    case '-' => Set((x - 1, y), (x + 1, y))
    case 'L' => Set((x, y - 1), (x + 1, y))
    case 'J' => Set((x, y - 1), (x - 1, y))
    case '7' => Set((x, y + 1), (x - 1, y))
    case 'F' => Set((x, y + 1), (x + 1, y))
    case '.' => Set()
    case 'S' =>
      Set((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).filter:
        (newX, newY) =>
          field.isDefinedAt(newX) && field(newY).isDefinedAt(newX) && connected(
            field,
            (newX, newY)
          ).contains(position)
    case _ => Set()

def findLoop(field: Field): Option[LazyList[Position]] =
  val start = findStart(field)
  // It doesn't matter which way we go (forwards or backwards),
  // so let's arbitrarly pick the first connection
  connected(field, start).headOption.map: initialPick =>
    val loop = LazyList.iterate((start, initialPick)): (previous, current) =>
      // This works because there are always 2 ways to go
      // and one of these is the previous pick
      val next = connected(field, current) - previous
      (current, next.head)

    start +: loop.map(_._2).takeWhile(_ != start)

val part1: Field => Option[Int] = findLoop.andThen(_.map(_.length / 2))

def part2(field: Field) =
  findLoop(field)
    .map(_.toSet)
    .map: loop =>
      def breaksLoop(x: Int, y: Int) =
        connected(field, (x, y)).contains((x, y - 1))

      field.indices
        .map: y =>
          val (_, count) = field(y).indices.foldLeft((false, 0)):
            case ((isInLoop, count), x) if loop.contains((x, y)) =>
              (isInLoop ^ breaksLoop(x, y), count)
            case ((true, count), x) =>
              (true, count + 1)
            case ((false, count), _) => (false, count)
          count
        .sum

@main def run: Unit =
  scala.io.Source
    .fromFile("./day10.input")
    .getLines()
    .toSeq
    // .pipe(part1.andThen(println))
    .pipe(part2.andThen(println))
