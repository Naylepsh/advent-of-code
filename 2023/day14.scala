package day14

import cats.syntax.all.*

import scala.annotation.tailrec
import scala.util.chaining.*

type Tile = '.' | '#' | 'O'
object Tile:
  def of: Char => Option[Tile] =
    case v @ ('.' | '#' | 'O') => Some(v)
    case _                     => None

type Platform = List[List[Tile]]
object Platform:
  val empty: Platform = List.empty[List[Tile]]

def parse(input: List[String]): Option[Platform] =
  val initial: Option[Platform] = Platform.empty.some
  input
    .foldLeft(initial): (acc, line) =>
      val xs: Option[List[Tile]] = line.toList.traverse(v => Tile.of(v))
      acc.flatMap(p => xs.map(_ :: p))
    .map(_.reverse)

def calculateNearestFreeSpots(platform: Platform): List[List[Int]] =
  val initial = platform.head.map(_ => 0)
  platform.zipWithIndex
    .foldLeft(initial :: Nil):
      case (acc, (row, idx)) =>
        val adjustedRow = row
          .zip(acc.head)
          .map:
            case ('.', previous) => previous
            case ('O', previous) => previous + 1
            case ('#', _)        => idx + 1
        adjustedRow :: acc
    .reverse
    .tail

def calculateLoad(platform: Platform) =
  val height = platform.length
  platform
    .zip(calculateNearestFreeSpots(platform))
    .foldLeft(0):
      case (acc, (row, spots)) =>
        acc + row
          .zip(spots)
          .foldLeft(0):
            case (acc, ('O', spot)) =>
              acc + (height - spot + 1)
            case (acc, _) => acc

@main def run: Unit =
  scala.io.Source
    .fromFile("./day14.input")
    .getLines()
    .toList
    .pipe(parse)
    .pipe(_.map(calculateLoad))
    // .pipe(solve(findReflectionIndex))
    .pipe(println)
