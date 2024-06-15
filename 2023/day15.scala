package day15

import scala.util.chaining.*
import scala.collection.mutable.{Map as MMap, LinkedHashMap as OrderedMap}

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
  val boxes  = MMap.empty[Hash, OrderedMap[String, Int]]

  parse(s).foreach:
    case add(key, value) =>
      boxes.updateWith(Hash(key)):
        case Some(values) =>
          values(key) = value.toInt
          Some(values)
        case None =>
          Some(OrderedMap(key -> value.toInt))
    case remove(key) =>
      boxes.updateWith(Hash(key)):
        case Some(values) =>
          values.remove(key)
          Some(values)
        case None => None

  (0 to 256).foldLeft(0): (acc, boxIdx) =>
    acc + (boxIdx + 1) * boxes
      .getOrElse(boxIdx, OrderedMap.empty)
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
