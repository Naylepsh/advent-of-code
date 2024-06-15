package day14

import cats.syntax.all.*

import scala.annotation.tailrec
import scala.util.chaining.*

type Platform = List[String]
object Platform:
  extension (platform: Platform)
    def tilt: Platform =
      platform.map: row =>
        "#+|[^#]+".r.findAllIn(row.mkString).map(_.sorted.reverse).mkString

    def rotateClockwise: Platform =
      platform.transpose.map(_.reverse.mkString)

    def rotateCounterClockwise: Platform =
      platform.map(_.reverse).transpose.map(_.mkString)

    def load: Int =
      platform.zipWithIndex
        .foldMap((row, i) => row.count(_ == 'O') * (platform.length - i))

  @tailrec
  def cycle(
      platform: Platform,
      i: Int,
      cache: Map[Platform, Int] = Map.empty
  ): Platform =
    cache.get(platform) match
      case Some(loopStartIdx) =>
        cache.map(_.swap)(loopStartIdx + i % (cache.size - loopStartIdx))
      case None =>
        cycle(
          platform.tilt.rotateClockwise.tilt.rotateClockwise.tilt.rotateClockwise.tilt.rotateClockwise,
          i - 1,
          cache.updated(platform, cache.size)
        )

  val part1: Platform => Int =
    _.rotateCounterClockwise.tilt.rotateClockwise.load

  val part2: Platform => Int =
    _.rotateCounterClockwise.pipe(cycle(_, 1_000_000_000).rotateClockwise.load)

@main def run: Unit =
  import Platform.*
  scala.io.Source
    .fromFile("./day14.input")
    .getLines()
    .toList
    // .pipe(part1)
    .pipe(part2)
    .pipe(println)
