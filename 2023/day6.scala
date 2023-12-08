import scala.util.chaining.*

def parse(lines: List[String]): List[(Long, Long)] =
  lines match
    case times :: distances :: Nil =>
      getNumbers(times).zip(getNumbers(distances))
    case _ => List.empty

private def getNumbers(line: String): List[Long] =
  val xs = line.split(":").tail.head.split("\\s+").toList match
    case "" :: tail => tail
    case xs         => xs
  xs.map(_.toLong)

def wayCount(time: Long, distance: Long): Long =
  // f(x) = -x^2 + tx - d > 0
  val delta = time * time - 4 * distance
  if delta < 0 then 0
  else if delta == 0 then (time / -2).abs
  else
    val deltaRoot = Math.sqrt(delta)
    val x1 = ((time - deltaRoot) / 2).toDouble
    val x2 = ((time + deltaRoot) / 2).toDouble
    val res = (x2.toInt - x1.toInt).abs
    if x1 % 1 == 0 && x2 % 1 == 0 then res - 1 else res

extension (xs: List[Long]) def product: Long = xs.foldLeft(1L)(_ * _)

def solve(lines: List[String]): Long =
  parse(lines)
    .map: (time, distance) =>
      wayCount(time, distance)
    .product

@main def runDay6: Unit =
  val lines = scala.io.Source.fromFile("./day6.input").getLines.toList
  // part 1
  lines.pipe(solve.andThen(println))
  // part 2
  lines.map(_.filterNot(_.isWhitespace)).pipe(solve.andThen(println))
