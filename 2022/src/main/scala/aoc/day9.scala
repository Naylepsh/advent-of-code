package aoc

import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

object day9 extends App:
  def cartesian[A](xs: List[A]): List[(A, A)] =
    for
      x <- xs
      y <- xs
    yield (x, y)

  def bind(lowerBound: Int, upperBound: Int)(x: Int): Int =
    x.max(lowerBound).min(upperBound)

  type Position = (Int, Int)
  object Position:
    def areWithinReach(a: Position, b: Position): Boolean =
      cartesian(List(-1, 0, 1)).find { case (dx, dy) =>
        (a._1 + dx, a._2 + dy) == b
      }.isDefined

    def follow(head: Position, tail: Position): Position =
      val (hx, hy) = head
      val (tx, ty) = tail

      (tx + bindDiff(hx - tx), ty + bindDiff(hy - ty))

    private val bindDiff = bind(lowerBound = -1, upperBound = 1)

  enum Direction:
    case Right, Up, Left, Down

  def solve(ropeLength: Int)(inputPath: String): Int =
    val ropePositions = List.fill(ropeLength)((0, 0)).toArray
    val tailPositions = Set[Position]((0, 0))

    def moveBy(dx: Int, dy: Int): Unit =
      val (hx, hy) = ropePositions.head
      ropePositions(0) = (hx + dx, hy + dy)

      1 until ropeLength foreach { knot =>
        val headPosition = ropePositions(knot - 1)
        val knotPosition = ropePositions(knot)

        if (!Position.areWithinReach(headPosition, knotPosition))
          ropePositions(knot) = Position.follow(headPosition, knotPosition)
      }

      tailPositions.add(ropePositions.last)

    def move(direction: Direction, steps: Int): Unit =
      val (dx, dy) = direction match
        case Direction.Right => (1, 0)
        case Direction.Up    => (0, 1)
        case Direction.Left  => (-1, 0)
        case Direction.Down  => (0, -1)

      1 to steps foreach { _ =>
        moveBy(dx, dy)
      }

    getFileContent(inputPath).foreach { input =>
      input match
        case s"R $steps" => move(Direction.Right, steps.toInt)
        case s"U $steps" => move(Direction.Up, steps.toInt)
        case s"L $steps" => move(Direction.Left, steps.toInt)
        case s"D $steps" => move(Direction.Down, steps.toInt)

    }

    tailPositions.size

  val solveA = solve(ropeLength = 2)
  val solveB = solve(ropeLength = 10)
  // println(solveA("./inputs/day9/data.txt"))
  println(solveB("./inputs/day9/data.txt"))
