package day13

import scala.util.chaining.*

type Pattern = List[List[Char]]

def parse(lines: List[String]): List[Pattern] =
  val (current, acc) = lines
    .foldLeft((List.empty[List[Char]], List.empty[Pattern])):
      case ((current, all), row) =>
        if row.isEmpty
        then (List.empty, current.reverse :: all)
        else ((row.toList :: current), all)
  (current.reverse :: acc).reverse

def findReflectionValue(
    pattern: Pattern,
    findRefIndex: Pattern => Option[Int]
): Int =
  findRefIndex(pattern)
    .map(_ * 100)
    .getOrElse(findRefIndex(pattern.transpose).get)

def findReflectionIndex(pattern: Pattern): Option[Int] =
  (1 until pattern.length).find: offset =>
    val (left, right) = pattern.splitAt(offset)
    left.reverse.zip(right).forall(_ == _)

def findSmudgedReflectionIndex(pattern: Pattern): Option[Int] =
  (1 until pattern.length).find: offset =>
    val (left, right) = pattern.splitAt(offset)
    left.reverse.zip(right).map(_.zip(_).count(_ != _)).sum == 1

def solve(findRefIndex: Pattern => Option[Int])(patterns: List[Pattern]): Long =
  patterns.foldLeft(0L)(_ + findReflectionValue(_, findRefIndex))

@main
def run: Unit =
  scala.io.Source
    .fromFile("./day13.input")
    .getLines()
    .toList
    .pipe(parse)
    // .pipe(solve(findReflectionIndex))
    .pipe(solve(findSmudgedReflectionIndex))
    .pipe(println)
