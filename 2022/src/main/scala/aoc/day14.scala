package aoc

object day14 extends App:
  enum Atom:
    case Rock, Sand, SandSource, Air
  object Atom:
    extension (atom: Atom)
      def show: String = atom match
        case Rock       => "#"
        case Sand       => "o"
        case SandSource => "+"
        case Air        => "."

      def isSolid: Boolean = atom match
        case Rock | Sand => true
        case _           => false

  type Cave = Array[Array[Atom]]
  object Cave:
    def apply(width: Int, height: Int): Cave =
      (0 to width)
        .map: _ =>
          (0 to height)
            .map: _ =>
              Atom.Air
            .toArray
        .toArray

    extension (cave: Cave)
      def addRockFormation(traces: List[Trace]): Unit =
        traces
          .zip(traces.tail)
          .foreach: (from, to) =>
            if from.x == to.x
            then
              (from.y.min(to.y).to(from.y.max(to.y))).foreach: h =>
                cave(h)(from.x) = Atom.Rock
            else
              (from.x.min(to.x).to(from.x.max(to.x))).foreach: w =>
                cave(from.y)(w) = Atom.Rock

      def takeOf(x: Int, y: Int): Option[Atom] =
        if 0 <= y && y < cave.length && 0 <= x && x < cave.head.length
        then Some(cave(y)(x))
        else None

      def addAt(x: Int, y: Int, atom: Atom): Unit =
        if 0 <= y && y < cave.length && 0 <= x && x < cave.head.length
        then cave(y)(x) = atom

      @annotation.tailrec
      def trickleSand(bottom: Int, fromX: Int, fromY: Int): Boolean =
        if fromY + 1 == bottom
        then
          addAt(fromX, fromY, Atom.Sand)
          false
        else
          val nextPosition = List(
            (fromX, fromY + 1),
            (fromX - 1, fromY + 1),
            (fromX + 1, fromY + 1)
          ).find: (x, y) =>
            cave.takeOf(x, y).map(_.isSolid) == Some(false)
          nextPosition match
            case None =>
              val atom = takeOf(fromX - 1, fromY + 1).flatMap: _ =>
                takeOf(fromX, fromY)
              atom match
                case Some(Atom.SandSource) => true
                case _ =>
                  addAt(fromX, fromY, Atom.Sand)
                  false
            case Some(x, y) => trickleSand(bottom, x, y)

      @annotation.tailrec
      def trickleSandUntilSpills(
          bottom: Int,
          fromX: Int,
          fromY: Int,
          sandUnitsPlaced: Int = 0
      ): Int =
        if cave.trickleSand(bottom, fromX, fromY) then sandUnitsPlaced + 1
        else trickleSandUntilSpills(bottom, fromX, fromY, sandUnitsPlaced + 1)

      def show(skipX: Int, limitX: Int, skipY: Int, limitY: Int): String =
        cave
          .drop(skipX)
          .take(limitX)
          .map: line =>
            line.drop(skipY).take(limitY).map(_.show).mkString("")
          .mkString("\n")

      def debug(x: Int, y: Int): Unit =
        println(s"Debugging at ($x, $y)")
        scala.io.StdIn.readLine()
        val atom = cave.takeOf(x, y).get
        cave.addAt(x, y, Atom.Sand)
        val vis = cave
          .drop((y - 5).max(0))
          .take((10).min(cave.length))
          .map: line =>
            line
              .drop((x - 5).max(0))
              .take((10).min(line.length))
              .map(_.show)
              .mkString("")
          .mkString("\n")
        println(vis)
        println()
        cave.addAt(x, y, atom)

  case class Trace(x: Int, y: Int)
  object Trace:
    def fromLine(line: String): List[Trace] =
      line
        .split(" -> ")
        .map: rawTrace =>
          rawTrace.split(",") match
            case Array(x, y) => Trace(x.toInt, y.toInt)
        .toList

  import Cave.*

  val cave = Cave(1000, 1000)

  // this solves only the 2nd part
  val bottom = getFileContent("./inputs/day14/data.txt")
    // val y = getFileContent("./inputs/day14/example.txt")
    .map: line =>
      Trace.fromLine(line)
    .tapEach: traces =>
      cave.addRockFormation(traces)
    .flatMap: traces =>
      traces.map(_.y)
    .toList
    .max + 2

  cave.addAt(500, 0, Atom.SandSource)

  val units = cave.trickleSandUntilSpills(bottom, 500, 0)
  println(units)
