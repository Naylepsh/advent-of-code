package day17

import scala.collection.mutable.{PriorityQueue, Map}
import scala.util.chaining.*

enum Direction:
  case Up, Right, Down, Left

  def rotateClockwise: Direction =
    Direction.fromOrdinal((this.ordinal + 1) % 4)

  def rotateCounterClockwise: Direction =
    Direction.fromOrdinal((this.ordinal + 3) % 4)

case class Point(x: Int, y: Int):
  val move: Direction => Point =
    case Direction.Up    => this.copy(y = y - 1)
    case Direction.Down  => this.copy(y = y + 1)
    case Direction.Left  => this.copy(x = x - 1)
    case Direction.Right => this.copy(x = x + 1)

type Island = Array[Array[Int]]
object Island:
  def parse(input: List[String]): Island =
    input.map(_.map(_.asDigit).toArray).toArray

  extension (self: Array[Array[Int]])
    def bottomRightCorner: Point =
      Point(x = self.head.length - 1, y = self.length - 1)

    def at(point: Point): Option[Int] =
      if self.isDefinedAt(point.y) && self(point.y).isDefinedAt(point.x) then
        Some(self(point.y)(point.x))
      else None

export Island.*

case class State(point: Point, direction: Direction, streak: Int):
  def moveStraight: State =
    State(point.move(direction), direction, streak + 1)

  def moveLeft: State =
    val dir = direction.rotateClockwise
    State(point.move(dir), dir, streak = 1)

  def moveRight: State =
    val dir = direction.rotateCounterClockwise
    State(point.move(dir), dir, streak = 1)

trait Crucible:
  def next(state: State, island: Island): List[State]
  def canStop(state: State): Boolean

object NormalCrucible extends Crucible:
  def next(state: State, island: Island): List[State] =
    List(state.moveStraight, state.moveLeft, state.moveRight).filter: s =>
      island.at(s.point).isDefined && state.streak <= 3
  def canStop(state: State): Boolean = true

object UltraCrucible extends Crucible:
  def next(state: State, island: Island): List[State] =
    if state.streak < 4
    then List(state.moveStraight)
    else
      List(state.moveStraight, state.moveLeft, state.moveRight).filter: s =>
        island.at(s.point).isDefined && state.streak <= 10
  def canStop(state: State): Boolean = 4 <= state.streak && state.streak <= 10

def djikstra(island: Island, crucible: Crucible): Int =
  val initial           = State(Point(0, 0), Direction.Right, 0)
  val minHeatLoss       = Map(initial -> 0)
  given Ordering[State] = Ordering.by(minHeatLoss).reverse
  val queue             = PriorityQueue(initial)

  while queue.nonEmpty do
    val current = queue.dequeue()
    crucible
      .next(current, island)
      .filterNot(minHeatLoss.contains)
      .foreach: nextState =>
        minHeatLoss(nextState) =
          island.at(nextState.point).getOrElse(0) + minHeatLoss(current)
        queue.enqueue(nextState)

  minHeatLoss.view
    .filterKeys(s => s.point == island.bottomRightCorner && crucible.canStop(s))
    .values
    .min

def part1(island: Island): Int = djikstra(island, NormalCrucible)
def part2(island: Island): Int = djikstra(island, UltraCrucible)

@main def run: Unit =
  scala.io.Source
    .fromFile("./day17.input")
    .getLines()
    .toList
    .pipe(Island.parse)
    .pipe(part2)
    .pipe(println)
