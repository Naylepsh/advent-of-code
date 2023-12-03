package aoc

import scala.annotation.tailrec

object day1 extends App:
  def computeStocks(rows: List[String]): List[Int] =
    @tailrec
    def compute(
        rows: List[String],
        currentStock: Int,
        acc: List[Int]
    ): List[Int] = rows match
      case head :: next =>
        if (head == "")
          compute(next, 0, currentStock :: acc)
        else
          compute(next, head.toInt + currentStock, acc)
      case Nil => acc

    compute(rows, 0, List[Int]()).reverse

  def solve(inputPath: String, topCount: Int) = {
    val stocks = computeStocks(getFileContent(inputPath).toList)
    val sorted = stocks.zipWithIndex.sortBy { case (stocks, owner) =>
      stocks
    }(Ordering.Int.reverse)

    sorted.take(topCount)
  }

  def solveInit(inputPath: String, topCount: Int) = {
    val topCarriers = solve(inputPath, topCount)

    println(topCarriers)
  }

  def solveActual(inputPath: String, topCount: Int) = {
    val topCarriers = solve(inputPath, topCount)

    println(topCarriers.map(_._1).sum)
  }

  // solveInit("./inputs/day1/data.txt", 1)
  day1.solveActual("./inputs/day1/data.txt", 3)
