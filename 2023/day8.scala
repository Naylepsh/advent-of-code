import scala.annotation.tailrec
import scala.util.chaining.*

object day8:
  enum Direction:
    case L, R

  case class Path(label: String, left: String, right: String)

  def parse(lines: List[String]): (List[Direction], List[Path]) =
    lines match
      case rawDirections :: _ :: rawPaths =>
        (
          rawDirections.map(c => Direction.valueOf(c.toString)).toList,
          parsePaths(rawPaths)
        )

  private def parsePaths(lines: List[String]): List[Path] =
    parsePaths(lines, List.empty)

  @tailrec
  private def parsePaths(lines: List[String], acc: List[Path]): List[Path] =
    lines match
      case Nil => acc
      case line :: tail =>
        val Array(label, rawMoves) = line
          .filterNot(c => c.isWhitespace || List('(', ')').contains(c))
          .split("=")
        val Array(left, right) = rawMoves.split(",")
        parsePaths(tail, Path(label, left, right) :: acc)

  extension [T](xs: List[T])
    def rotate: List[T] =
      xs match
        case Nil          => Nil
        case head :: tail => tail ::: List(head)

  def move(
      startLabel: String,
      endLabel: String,
      paths: List[Path],
      directions: List[Direction]
  ): Int =
    @tailrec
    def aux(
        currentLabel: String,
        directions: List[Direction],
        stepCount: Int
    ): Int =
      if currentLabel == endLabel then stepCount
      else
        val path = paths.find(_.label == currentLabel).get
        val nextLabel = directions.head match
          case Direction.L => path.left
          case Direction.R => path.right
        aux(nextLabel, directions.rotate, stepCount + 1)

    aux(startLabel, directions, 0)

@main def runDay8 =
  import day8.*

  val lines = scala.io.Source.fromFile("./day8.input").getLines.toList

  // part 1
  val (directions, paths) = parse(lines)
  move("AAA", "ZZZ", paths, directions).pipe(println)