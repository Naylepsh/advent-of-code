package aoc

import scala.annotation.tailrec

object day5 extends App:
  case class Instruction(count: Int, from: Int, to: Int)
  def rowToInstruction(row: String): Instruction =
    "[0-9]+".r.findAllIn(row).toList match
      case count :: from :: to :: Nil =>
        Instruction(count.toInt, from.toInt, to.toInt)

  def rowToCrates(row: String): List[String] =
    "\\[([A-Z]| )\\]".r.findAllIn(row).toList.map { case s"[$symbol]" =>
      symbol
    }

  def buildCrates(rows: List[List[String]]): List[List[String]] =
    rows.transpose.map(_.filterNot(_ == " "))

  def solve(
      pick: (List[String], Int) => List[String]
  )(cratesInputPath: String, instructionsInputPath: String): String =
    val crateRows = getFileContent(cratesInputPath).map(rowToCrates).toList
    val crates = buildCrates(crateRows)

    val instructions =
      getFileContent(instructionsInputPath).map(rowToInstruction).toList

    @tailrec
    def run(
        instructionsLeft: List[Instruction],
        currentCrates: List[List[String]]
    ): List[List[String]] =
      instructionsLeft match
        case Instruction(count, from, to) :: next =>
          val taken = pick(currentCrates(from - 1), count)

          val newColumns = currentCrates.zipWithIndex.map {
            case (column, index) =>
              if (index + 1 == from)
                column.drop(count)
              else if (index + 1 == to)
                taken ++ column
              else
                column
          }
          run(next, newColumns)

        case Nil => currentCrates

    run(instructions, crates).map(_.head).mkString("")

  val solveInitial = solve((xs, n) => xs.take(n).reverse)
  val solveActual = solve((xs, n) => xs.take(n))

  // val res = solveInitial("./inputs/day5/data-crates.txt", "./inputs/day5/data-instructions.txt")
  val res = solveActual(
    "./inputs/day5/data-crates.txt",
    "./inputs/day5/data-instructions.txt"
  )
  println(res)
