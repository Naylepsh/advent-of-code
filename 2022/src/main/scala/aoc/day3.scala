package aoc

import scala.annotation.tailrec

object day3 extends App:
  val priorities = (('a' to 'z') ++ ('A' to 'Z')).zipWithIndex.map {
    case (char, index) => (char, index + 1)
  }.toMap

  def uniqueChars(string: String): Set[Char] =
    string.toCharArray.toSet

  def uniqueChars(strings: List[String]): Set[Char] =
    @tailrec
    def run(stringsLeft: List[String], acc: Set[Char]): Set[Char] =
      stringsLeft match
        case head :: next => run(next, uniqueChars(head).intersect(acc))
        case Nil          => acc

    run(strings.tail, uniqueChars(strings.head))

  trait Solution:
    val groupSize: Int
    def getCommonLetters(strs: List[String]): List[Char]

    def solve(inputPath: String): Int =
      getFileContent(inputPath).toList
        .grouped(groupSize)
        .flatMap(getCommonLetters)
        .map(priorities)
        .sum

  val initial = new Solution:
    override val groupSize: Int = 1

    override def getCommonLetters(strs: List[String]): List[Char] =
      // strs is a list of exactly 1 element
      val (a, b) = partition(strs.head)
      uniqueChars(List(a, b)).toList

    private def partition(str: String): (String, String) =
      val mid = str.length / 2
      (str.substring(0, mid), str.substring(mid))

  val actual = new Solution:
    override val groupSize: Int = 3

    override def getCommonLetters(strings: List[String]): List[Char] =
      uniqueChars(strings).toList

  // val res = initial.solve("./inputs/day3/data.txt")
  val res = actual.solve("./inputs/day3/data.txt")
  println(res)
