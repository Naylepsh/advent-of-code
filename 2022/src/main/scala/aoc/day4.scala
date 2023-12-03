package aoc

object day4 extends App:
  case class Assigment(start: Int, end: Int)
  object Assigment:
    def fullyContains(a: Assigment, b: Assigment): Boolean =
      a.start <= b.start && a.end >= b.end

    def isEitherSubset(a: Assigment, b: Assigment): Boolean =
      fullyContains(a, b) || fullyContains(b, a)

    def overlap(a: Assigment, b: Assigment): Boolean =
      a.start <= b.start && b.start <= a.end

    def isEitherOverlapping(a: Assigment, b: Assigment): Boolean =
      overlap(a, b) || overlap(b, a)

  def parseRow(row: String): List[Assigment] =
    row.split(",").map(parseAssignment).toList

  def parseAssignment(string: String): Assigment =
    string.split("-").toList match
      case start :: end :: Nil => Assigment(start.toInt, end.toInt)

  def solve(filter: (Assigment, Assigment) => Boolean)(inputPath: String): Int =
    getFileContent(inputPath)
      .map(parseRow)
      .filter {
        case a :: b :: Nil => filter(a, b)
        case _             => false
      }
      .length

  val solveInit = solve(Assigment.isEitherSubset)
  val solveActual = solve(Assigment.isEitherOverlapping)

  // val res = solveInit("./inputs/day4/data.txt")
  val res = solveActual("./inputs/day4/data.txt")
  println(res)
