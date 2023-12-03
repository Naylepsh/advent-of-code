package aoc

import scala.collection.mutable.{Map, Queue, ArrayBuffer}

object day11 extends App:
  def parseVariable(variable: String, default: BigDecimal): BigDecimal =
    if (variable == "old") default else variable.toInt

  def parseOperation(input: String): BigDecimal => BigDecimal =
    input.trim match
      case s"$a + $b" =>
        (x: BigDecimal) => parseVariable(a, x) + parseVariable(b, x)
      case s"$a * $b" =>
        (x: BigDecimal) => parseVariable(a, x) * parseVariable(b, x)

  def parseDiv(input: String): Int =
    input.trim match
      case s"$rest by $div" => div.toInt

  def parseInstruction(input: String): Int =
    input.trim match
      case s"$rest monkey $i" => i.toInt

  class Monkey(
      val items: Queue[BigDecimal],
      operation: BigDecimal => BigDecimal,
      val div: Int,
      ifTrue: Int,
      ifFalse: Int
  ):
    def decideTarget(
        item: BigDecimal,
        decay: BigDecimal = 1
    ): (BigDecimal, Int) =
      val newWorryLevel = operation(item) / decay
      val target =
        if (test(newWorryLevel))
          ifTrue
        else
          ifFalse

      (newWorryLevel, target)

    private def test(item: BigDecimal): Boolean =
      item % div == 0

  def parseInput(inputPath: String) =
    getFileContent(inputPath).grouped(7).map { data =>
      val startingItems =
        data(1).split(":").last.split(", ").map(x => BigDecimal(x.trim.toInt))
      val operation = parseOperation(data(2).split("=").last)
      val test = parseDiv(data(3))
      val ifTrue = parseInstruction(data(4))
      val ifFalse = parseInstruction(data(5))

      new Monkey(Queue.from(startingItems), operation, test, ifTrue, ifFalse)
    }

  def runRound(
      monkeys: Array[Monkey],
      inspects: ArrayBuffer[Int],
      decay: Int,
      divProduct: Int
  ) =
    monkeys.zipWithIndex.foreach { case (monkey, index) =>
      for (_ <- 1 to monkey.items.length)
        val item = monkey.items.dequeue() % divProduct
        val (newWorryLevel, target) = monkey.decideTarget(item, decay)
        monkeys(target).items.enqueue(newWorryLevel)
        inspects(index) += 1
    }

  def solve[A](decay: Int)(monkeys: Array[Monkey], rounds: Int) =
    val divProduct = monkeys.map(_.div).product
    val inspects = ArrayBuffer.fill(monkeys.length)(0)
    for (_ <- 1 to rounds)
      runRound(monkeys, inspects, decay, divProduct)
    inspects.sorted(Ordering.Int.reverse).toList match
      case x :: y :: rest => BigDecimal(x) * BigDecimal(y)

  val monkeys = parseInput("./inputs/day11/data.txt").toArray
  val solveA = solve(decay = 3)
  val solveB = solve(decay = 1)
  // println(solveA(monkeys, 20))
  println(solveB(monkeys, 10000))
