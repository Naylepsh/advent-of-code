import scala.annotation.tailrec
import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration.Duration
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
      endSuffix: String,
      paths: List[Path],
      directions: List[Direction]
  ): Long =
    @tailrec
    def aux(
        currentLabel: String,
        directions: List[Direction],
        stepCount: Long
    ): Long =
      if currentLabel.endsWith(endSuffix) then stepCount
      else
        val path = paths.find(_.label == currentLabel).get
        val nextLabel = directions.head match
          case Direction.L => path.left
          case Direction.R => path.right
        aux(nextLabel, directions.rotate, stepCount + 1)

    aux(startLabel, directions, 0)

  def gcd(a: Long, b: Long): Long   = if b == 0 then a else gcd(b, a % b)
  def lcm(a: Long, b: Long): Long   = (a * b) / gcd(a, b)
  def lcm(numbers: Seq[Long]): Long = numbers.reduce(lcm(_, _))

  def solveForGhost(paths: List[Path], directions: List[Direction])(using
  ExecutionContext): Long =
    paths
      .filter(_.label.endsWith("A"))
      .map: path =>
        Future:
          move(path.label, "Z", paths, directions)
      .pipe(Future.sequence)
      .pipe(Await.result(_, Duration.Inf))
      .pipe(lcm)

@main def runDay8 =
  import day8.*

  val lines               = scala.io.Source.fromFile("./day8.input").getLines.toList
  val (directions, paths) = parse(lines)

  // part 1
  move("AAA", "ZZZ", paths, directions).pipe(println)

  /**
   * part 2
   * The only reason this works is because the input is "nice":
   *  - all A -> Z paths will form only one cycle. No branching to other Zs
   *  - path sizes of A -> Z and Z -> Z are the same
   * If inputs were any different, this would work.
   */
  import ExecutionContext.Implicits.global
  solveForGhost(paths, directions).pipe(println)
