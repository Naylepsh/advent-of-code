package day18

import cats.syntax.all.*
import util.chaining.*

case class Point(x: Long, y: Long)
object Point:
  val zero: Point = Point(0, 0)

  def shoelace(points: List[Point]): Long =
    points
      .zip(points.tail :+ points.head)
      .map((a, b) => (a.x * b.y) - (a.y * b.x))
      .sum
      .abs
      .pipe(_ / 2)

enum Direction:
  case Right, Down, Left, Up

object Direction:
  def apply(s: Char): Option[Direction] = s match
    case 'U' => Some(Up)
    case 'R' => Some(Right)
    case 'D' => Some(Down)
    case 'L' => Some(Left)
    case _   => None

case class Instruction(direction: Direction, meters: Long)
object Instruction:
  def parse(s: List[String]): Option[List[Instruction]] =
    s.traverse:
      case s"$dir $meters $_" =>
        for
          d <- Direction(dir.head)
          m <- meters.toLongOption
        yield Instruction(d, m)
      case _ => None

  def parse2(s: List[String]): Option[List[Instruction]] =
    s.traverse:
      case s"$_ $_ (#$input)" =>
        Instruction(
          Direction.fromOrdinal(input.last.asDigit),
          Integer.parseInt(input.take(5), 16)
        ).some
      case _ => None

  def toPoints(instructions: List[Instruction]): List[Point] =
    instructions
      .foldLeft(Point.zero :: Nil):
        case (acc, Instruction(dir, meters)) =>
          val point = acc.head
          val p = dir match
            case Direction.Up    => point.copy(y = point.y - meters)
            case Direction.Right => point.copy(x = point.x + meters)
            case Direction.Down  => point.copy(y = point.y + meters)
            case Direction.Left  => point.copy(x = point.x - meters)
          (p :: acc)
      .reverse

  def perimeter(instructions: List[Instruction]): Long =
    instructions.map(_.meters).sum + 1L

class Trench:
  def run(instructions: List[Instruction]) =
    /** TL;DR The area computation between shoelace and the problem differs due
      * to cartesian coordinates vs integer coordinates.
      *
      * Shoelace: (0, 0) -> (0,6) -> (1, 6) -> (1, 1) == 6
      *
      * The problem: (0, 0) -> (0, 6) == 7
      *
      * Thus the shenanigans with perimeter
      */
    Instruction.toPoints(instructions).pipe(Point.shoelace) +
      Instruction.perimeter(instructions) / 2 + 1L

@main def run: Unit =
  scala.io.Source
    .fromFile("./day18.input")
    .getLines()
    .toList
    // .pipe(Instruction.parse)
    .pipe(Instruction.parse2)
    .pipe(_.map(Trench().run))
    .pipe(println)
