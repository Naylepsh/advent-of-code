package aoc

import io.circe._
import io.circe.parser._

object day13 extends App:
  enum Comparison:
    case Lower, Equal, Greater

  def toNumberUnsafe(json: Json): Int =
    json.asNumber.flatMap(_.toInt).get

  def compare(left: Json, right: Json): Comparison =
    if (left.isNumber && right.isNumber)
      val a = toNumberUnsafe(left)
      val b = toNumberUnsafe(right)

      val res =
        if (a == b)
          Comparison.Equal
        else if (a > b)
          Comparison.Greater
        else
          Comparison.Lower
      res
    else if (left.isNumber && right.isArray)
      compare(Json.fromValues(List(left)), right)
    else if (left.isArray && right.isNumber)
      compare(left, Json.fromValues(List(right)))
    else
      val a = left.asArray.get
      val b = right.asArray.get

      if (a.isEmpty && b.isEmpty) Comparison.Equal
      else
        var i = 0
        var result: Option[Comparison] = None
        while (result.isEmpty)
          if (i >= a.length && i >= b.length) result = Some(Comparison.Equal)
          else if (i >= a.length) result = Some(Comparison.Lower)
          else if (i >= b.length) result = Some(Comparison.Greater)
          else
            val comparison = compare(a(i), b(i))
            if (comparison != Comparison.Equal)
              result = Some(comparison)
          i += 1

        result.get

  def isInRightOrder(left: Json, right: Json) =
    compare(left, right) == Comparison.Lower

  def parseInput(inputPath: String) =
    getFileContent(inputPath).grouped(3).map {
      _.toList match
        case left :: right :: rest => (left, right)
    }

  def solveA(pairs: List[(String, String)]) =
    pairs
      .map { case (a, b) =>
        (parse(a), parse(b)) match
          case (Right(left), Right(right)) => isInRightOrder(left, right)
      }
      .zipWithIndex
      .collect { case (true, index) =>
        index + 1
      }
      .sum

  def solveB(pairs: List[(String, String)]) =
    val dividerPackets = List("[[2]]", "[[6]]")
    val sortedPackets = (pairs
      .flatMap { case (a, b) =>
        List(a, b)
      } ::: dividerPackets)
      .sortWith { case (a, b) =>
        (parse(a), parse(b)) match
          case (Right(left), Right(right)) => isInRightOrder(left, right)
      }
    dividerPackets.map(packet => sortedPackets.indexOf(packet) + 1).product

  val input = parseInput("./inputs/day13/data.txt").toList
  // val input = parseInput("./inputs/day13/example.txt").toList
  // println(solveA(input))
  println(solveB(input))
