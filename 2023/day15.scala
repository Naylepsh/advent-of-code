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

def part2(s: String): Int =
  val add    = "(.+)=(\\d+)".r
  val remove = "(.+)-".r
  val boxes  = Map.empty[Hash, LinkedHashMap[String, Int]]

  parse(s).foreach:
    case add(key, value) =>
      val hash = Hash(key)
      boxes.get(hash) match
        case Some(box) => box(key) = value.toInt
        case None      => boxes.update(hash, LinkedHashMap(key -> value.toInt))
    case remove(key) =>
      boxes.get(Hash(key)).foreach(_.remove(key))

  (0 to 256).foldLeft(0): (acc, boxIdx) =>
    acc + (boxIdx + 1) * boxes
      .getOrElse(boxIdx, LinkedHashMap.empty)
      .zipWithIndex
      .foldLeft(0):
        case (acc, ((_, value), idx)) =>
          acc + (idx + 1) * value

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
