package day11

import scala.annotation.tailrec
import scala.util.chaining.*

type ExpandedRows    = List[Int]
type ExpandedColumns = List[Int]
type ExpandedSpaces  = (ExpandedRows, ExpandedColumns)
object ExpandedSpaces:
  extension (spaces: ExpandedSpaces)
    def rows    = spaces._1
    def columns = spaces._2

type Galaxy = (Int, Int)
object Galaxy:
  extension (galaxy: Galaxy)
    def row: Int    = galaxy._1
    def column: Int = galaxy._2

    def moveRow(using offset: Int): Galaxy =
      Galaxy(galaxy.column, galaxy.row + offset)
    def moveColumn(using offset: Int): Galaxy =
      Galaxy(galaxy.column + offset, galaxy.row)

    def distanceTo(other: Galaxy): Int =
      (galaxy.row - other.row).abs + (galaxy.column - other.column).abs

  def apply(column: Int, row: Int): Galaxy = (row, column)

type Galaxies = List[Galaxy]
object Galaxies:
  import Galaxy.*
  import ExpandedSpaces.*

  val empty = List.empty[Galaxy]

  def parse(space: List[String]): Galaxies = parse(space, empty, 0)

  @tailrec
  private def parse(space: List[String], acc: Galaxies, y: Int): Galaxies =
    space match
      case Nil => acc
      case head :: tail =>
        val rowAcc = head.zipWithIndex
          .foldLeft(empty):
            case (acc, ('#', x)) => Galaxy(x, y) :: acc
            case (acc, _)        => acc
        parse(tail, rowAcc ::: acc, y + 1)

  def findExpandedSpaces(
      galaxies: Galaxies,
      maxSpaceColumn: Int,
      maxSpaceRow: Int
  ): ExpandedSpaces =
    val expandedRows = (0 to maxSpaceRow).toList.filter: row =>
      galaxies.count(_.row == row) == 0
    val expandedColumns = (0 to maxSpaceColumn).toList.filter: column =>
      galaxies.count(_.column == column) == 0
    (expandedRows, expandedColumns)

  def extend(galaxies: Galaxies, expandedSpaces: ExpandedSpaces)(using
      offset: Int
  ): Galaxies =
    extendRows(galaxies, expandedSpaces.rows.sorted(Ordering[Int].reverse))
      .pipe(
        extendColumns(_, expandedSpaces.columns.sorted(Ordering[Int].reverse))
      )

  @tailrec
  private def extendRows(
      galaxies: Galaxies,
      expandedRows: ExpandedRows
  )(using offset: Int): Galaxies =
    expandedRows match
      case Nil => galaxies
      case row :: tail =>
        val extendedGalaxies = galaxies.map: galaxy =>
          if galaxy.row > row then galaxy.moveRow else galaxy
        extendRows(extendedGalaxies, tail)

  @tailrec
  private def extendColumns(
      galaxies: Galaxies,
      expandedColumns: ExpandedColumns
  )(using offset: Int): Galaxies =
    expandedColumns match
      case Nil => galaxies
      case column :: tail =>
        val extendedGalaxies = galaxies.map: galaxy =>
          if galaxy.column > column then galaxy.moveColumn else galaxy
        extendColumns(extendedGalaxies, tail)

def part(space: List[String])(using offset: Int): Long =
  import Galaxy.*

  val galaxies = Galaxies.parse(space)
  val expandedSpaces = Galaxies.findExpandedSpaces(
    galaxies,
    space.head.length - 1,
    space.length - 1
  )

  Galaxies
    .extend(galaxies, expandedSpaces)
    .pipe: galaxies =>
      galaxies
        .combinations(2)
        .foldLeft(0L):
          case (acc, lhs :: rhs :: _) => acc + lhs.distanceTo(rhs)

def part1(space: List[String]): Long = part(space)(using 1)
def part2(space: List[String]): Long = part(space)(using 999_999)

@main
def run: Unit =
  scala.io.Source
    .fromFile("./day11.input")
    .getLines()
    .toList
    // .pipe(part1.andThen(println))
    .pipe(part2.andThen(println))
