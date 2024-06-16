package day16

import scala.util.chaining.*
import scala.collection.mutable.{Map, Queue}

enum Direction:
  case Up, Right, Down, Left
object Direction:
  extension (self: Direction)
    def rotateClockwise: Direction =
      Direction.fromOrdinal((self.ordinal + 1) % 4)

    def rotateCounterClockwise: Direction =
      Direction.fromOrdinal((self.ordinal + 3) % 4)

def move(x: Int, y: Int, direction: Direction): (Int, Int) = direction match
  case Direction.Up    => x       -> (y - 1)
  case Direction.Right => (x + 1) -> y
  case Direction.Down  => x       -> (y + 1)
  case Direction.Left  => (x - 1) -> y

def nextPositions(
    x: Int,
    y: Int,
    direction: Direction,
    tiles: List[String]
): List[(Int, Int, Direction)] =
  import Direction.*

  if !tiles.isDefinedAt(y) || !tiles(y).isDefinedAt(x) then List.empty
  else
    val directions = (tiles(y)(x), direction) match
      case ('.', _) =>
        List(direction)
      case ('-', Direction.Left | Direction.Right) =>
        List(direction)
      case ('-', Direction.Up | Direction.Down) =>
        List(Direction.Left, Direction.Right)
      case ('|', Direction.Up | Direction.Down) =>
        List(direction)
      case ('|', Direction.Left | Direction.Right) =>
        List(Direction.Up, Direction.Down)
      case ('/', Direction.Left | Direction.Right) =>
        List(rotateCounterClockwise(direction))
      case ('/', Direction.Up | Direction.Down) =>
        List(rotateClockwise(direction))
      case ('\\', Direction.Left | Direction.Right) =>
        List(rotateClockwise(direction))
      case ('\\', Direction.Up | Direction.Down) =>
        List(rotateCounterClockwise(direction))
      case _ =>
        List.empty
    directions.map: direction =>
      val (newX, newY) = move(x, y, direction)
      (newX, newY, direction)

class BeamTracker(tiles: List[String]):
  private val cache          = Map.empty[(Int, Int, Direction), Boolean]
  private val energizedTiles = Map.empty[(Int, Int), Boolean]

  private def trackBeam(x: Int, y: Int, direction: Direction): Unit =
    val queue = Queue((x, y, direction))

    while queue.nonEmpty do
      val (x, y, direction) = queue.dequeue()
      cache((x, y, direction)) = true

      nextPositions(x, y, direction, tiles).foreach: args =>
        energizedTiles((x, y)) = true
        if !cache.contains(args) then queue.enqueue(args)

  def part1: Int =
    trackBeam(0, 0, Direction.Right)
    energizedTiles.size

  def part2: Int =
    inputsAroundEdges.foldLeft(0): (acc, startingInput) =>
      cache.clear()
      energizedTiles.clear()
      trackBeam.tupled(startingInput)
      acc.max(energizedTiles.size)

  private val inputsAroundEdges =
    val startTop = (0 until tiles(0).length).map(x => (x, 0, Direction.Down))
    val startBottom =
      (0 until tiles(0).length).map(x => (x, tiles.length - 1, Direction.Up))
    val startLeft = (0 until tiles.length).map(y => (0, y, Direction.Right))
    val startRight =
      (0 until tiles.length).map(y => (tiles(0).length - 1, y, Direction.Left))
    startTop ++ startBottom ++ startLeft ++ startRight

  private def debug: Unit =
    tiles.zipWithIndex.foreach: (row, y) =>
      (0 until row.length)
        .map(x => if energizedTiles.contains((x, y)) then '#' else '.')
        .mkString
        .pipe(println)

@main def run: Unit =
  scala.io.Source
    .fromFile("./day16.input")
    .getLines()
    .toList
    .pipe(new BeamTracker(_).part2)
    .pipe(println)
