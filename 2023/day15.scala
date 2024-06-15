package day15

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Map
import scala.util.chaining.*

type Hash = Int
object Hash:
  def apply(s: String): Hash =
    s.toList.foldLeft(0): (acc, char) =>
      ((char.toInt + acc) * 17 % 256)

def parse(s: String): List[String] =
  s.split(",").toList

def part1(s: String): Int =
  parse(s).map(Hash(_)).foldLeft(0)(_ + _)

type Boxes = Map[Hash, LinkedHashMap[String, Int]]
object Boxes:
  def empty: Boxes = Map.empty[Hash, LinkedHashMap[String, Int]]

  extension (self: Boxes)
    def add(key: String, value: Int): Unit =
      val hash = Hash(key)
      self.get(hash) match
        case Some(box) => box(key) = value
        case None      => self.update(hash, LinkedHashMap(key -> value))

    def remove(key: String): Unit =
      self.get(Hash(key)).foreach(_.remove(key))

    def focusingPower: Int =
      (0 to 256).foldLeft(0): (acc, boxIdx) =>
        acc + (boxIdx + 1) * self
          .getOrElse(boxIdx, LinkedHashMap.empty)
          .zipWithIndex
          .foldLeft(0):
            case (acc, ((_, value), idx)) =>
              acc + (idx + 1) * value

def part2(s: String): Int =
  val add    = "(.+)=(\\d+)".r
  val remove = "(.+)-".r
  val boxes  = Boxes.empty

  import Boxes.*

  parse(s).foreach:
    case add(key, value) => boxes.add(key, value.toInt)
    case remove(key)     => boxes.remove(key)

  boxes.focusingPower

@main
def run: Unit =
  scala.io.Source
    .fromFile("./day15.input")
    .getLines()
    .toList
    .head
    // .pipe(part1)
    .pipe(part2)
    .pipe(println)
