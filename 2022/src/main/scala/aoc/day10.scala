package aoc

import scala.collection.mutable.{ListBuffer, ArrayBuffer}

object day10 extends App:
  def signalStrength(cycleValues: Array[Int])(cycleNumber: Int): Int =
    cycleValues(cycleNumber - 1) * cycleNumber

  def computeCycleValues(inputPath: String): Array[Int] =
    val cycleValues = ListBuffer(1)
    var currentValue = cycleValues.head

    getFileContent(inputPath).foreach { command =>
      command match
        case "noop" => cycleValues.addOne(currentValue)
        case s"addx $n" =>
          val newValue = currentValue + n.toInt
          cycleValues.addAll(List(currentValue, newValue))
          currentValue = newValue
    }

    cycleValues.toArray

  def solveA(cycleValues: Array[Int]) =
    List(20, 60, 100, 140, 180, 220)
      .map(signalStrength(cycleValues))
      .sum

  def solveB(cycleValues: Array[Int], screenSize: Int) =
    val monitor = ArrayBuffer.fill(cycleValues.length)("")

    def containsSprite(spriteIndex: Int, column: Int): Boolean =
      List(-1, 0, 1).find(delta => spriteIndex + delta == column).isDefined

    1 until cycleValues.length foreach { i =>
      val spriteIndex = cycleValues(i)
      val column = i % screenSize

      monitor(i) =
        if (containsSprite(spriteIndex, column))
          "#"
        else
          "."
    }

    val display =
      monitor.tail.grouped(screenSize).map(_.mkString("")).mkString("\n")
    display

  val cycleValues = computeCycleValues("./inputs/day10/data.txt")
  // println(solveA(cycleValues))
  val screenSize = 40
  println(solveB(cycleValues, screenSize))
