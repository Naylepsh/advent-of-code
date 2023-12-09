import scala.annotation.tailrec
import scala.util.chaining.*

object day9:
  def parse(line: String): List[Int] =
    line.split(" ").map(_.toInt).toList

  def parse(lines: List[String]): List[List[Int]] = lines.map(parse)

  private def diffs(history: List[Int]): List[Int] =
    history.tail.zip(history).map(_ - _)

  def prepareForExtrapolation(history: List[Int]): List[List[Int]] =
    history :: prepareForExtrapolation(history, List.empty)

  private def prepareForExtrapolation(
      history: List[Int],
      acc: List[List[Int]]
  ): List[List[Int]] =
    if history.filter(_ == 0).length == history.length then acc.reverse
    else
      val diff = diffs(history)
      prepareForExtrapolation(diff, diff :: acc)

  def extrapolate(differences: List[List[Int]]): Int =
    @tailrec
    def aux(xs: List[Int], difference: Int): Int =
      xs match
        case Nil => difference
        case head :: tail =>
          aux(tail, head + difference)

    aux(differences.map(_.last), 0)

  def solve(histories: List[List[Int]]): Int =
    histories
      .map: history =>
        extrapolate(prepareForExtrapolation(history))
      .sum

@main def runDay9 =
  import day9.*

  val lines = scala.io.Source.fromFile("./day9.input").getLines.toList
  lines.map(parse).pipe(solve.andThen(println))
