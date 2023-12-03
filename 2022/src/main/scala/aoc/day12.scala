package aoc

import scala.collection.mutable.{ArrayBuffer, Queue}

object day12 extends App:
  type TwoDimArray[A] = ArrayBuffer[ArrayBuffer[A]]
  type HeightMap = TwoDimArray[Char]
  def parseMap(inputPath: String): HeightMap =
    ArrayBuffer.from(
      getFileContent(inputPath).map(line =>
        ArrayBuffer.from(line.toCharArray())
      )
    )

  def create2dArray[A](width: Int, height: Int, zero: A): TwoDimArray[A] =
    ArrayBuffer.fill(width)(ArrayBuffer.fill(height)(zero))

  def isWithinBound(a: Int, lower: Int, upper: Int): Boolean =
    0 <= a && a < upper

  def heightValue(height: Char): Int =
    height match
      case 'S'   => 'a'.toInt
      case 'E'   => 'z'.toInt
      case other => other.toInt

  def canMoveToPosition(
      map: HeightMap,
      fromX: Int,
      fromY: Int,
      toX: Int,
      toY: Int
  ): Boolean =
    isWithinBound(toX, 0, map.length)
      && isWithinBound(toY, 0, map.head.length)
      && heightValue(map(fromX)(fromY)) + 1 >= heightValue(map(toX)(toY))

  def solve(map: HeightMap, startX: Int, startY: Int): Int =
    val (endX, endY) = indicesOfHeight(map, 'E').head
    val queue = Queue[(Int, Int)]((startX, startY))
    val visited = create2dArray(map.length, map.head.length, false)
    visited(startX)(startY) = true
    val distances = create2dArray(map.length, map.head.length, 0)
    distances(startX)(startY) = 0
    val moves = List((-1, 0), (1, 0), (0, -1), (0, 1))

    while (!queue.isEmpty)
      val (x, y) = queue.dequeue()

      moves.foreach { case (dx, dy) =>
        val newX = x + dx
        val newY = y + dy

        if (canMoveToPosition(map, x, y, newX, newY) && !visited(newX)(newY))
          queue.enqueue((newX, newY))
          visited(newX)(newY) = true
          distances(newX)(newY) = distances(x)(y) + 1
      }

    distances(endX)(endY)

  def indicesOfHeight(map: HeightMap, height: Char): List[(Int, Int)] =
    map.zipWithIndex
      .map { case (xs, i) =>
        (i, xs.indexOf(height))
      }
      .filter { case (x, y) =>
        y != -1
      }
      .toList

  def solveA(map: HeightMap): Int =
    val (startX, startY) = indicesOfHeight(map, 'S').head
    solve(map, startX, startY)

  def solveB(map: HeightMap): Int =
    (indicesOfHeight(map, 'S') ++ indicesOfHeight(map, 'a')).map {
      case (startX, startY) => solve(map, startX, startY)
    }.min

  val map = parseMap("./inputs/day12/data.txt")
  // println(solveA(map))
  println(solveB(map))
