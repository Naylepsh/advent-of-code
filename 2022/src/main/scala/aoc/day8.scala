package aoc

object day8 extends App:
  type TreeLine = Array[Int]
  object TreeLine:
    def isShorterThan(n: Int)(treeLine: TreeLine): Boolean =
      treeLine.count(_ < n) == treeLine.length

    def fromString(string: String): TreeLine = string.map(_.toInt - 48).toArray

  type Trees = Array[TreeLine]
  object Trees:
    def isOuter(trees: Trees, row: Int, column: Int): Boolean =
      val rs = rows(trees)
      val cs = columns(trees)

      row == 0 || row == rs - 1 || column == 0 || column == cs - 1

    def rows(trees: Trees): Int = trees.length

    def columns(trees: Trees): Int = trees.head.length

  trait Solution:
    def scoreIfInner(trees: Trees, row: Int, column: Int): Int
    def scoreIfOuter(trees: Trees): Int
    def merge(trees: Trees): Int

    def score(trees: Trees): Trees =
      val rs = Trees.rows(trees)
      val cs = Trees.columns(trees)

      ((0 until rs) map { row =>
        ((0 until cs) map { column =>
          if (Trees.isOuter(trees, row, column))
            scoreIfOuter(trees)
          else
            scoreIfInner(trees, row, column)
        }).toArray
      }).toArray

    def solve(inputPath: String): Int =
      val trees = getFileContent(inputPath).map(TreeLine.fromString).toArray
      merge(score(trees))

  val solutionA = new Solution:
    override def scoreIfInner(trees: Trees, row: Int, column: Int): Int =
      val rows = Trees.rows(trees)
      val columns = Trees.columns(trees)
      val up = trees.take(row).map(_(column))
      val down = trees.slice(row + 1, rows).map(_(column))
      val left = trees(row).take(column)
      val right = trees(row).slice(column + 1, columns)

      val isVisible = List(up, down, left, right)
        .filter(
          TreeLine.isShorterThan(trees(row)(column))
        )
        .length > 0

      isVisible match
        case true  => 1
        case false => 0

    override def scoreIfOuter(trees: Trees): Int = 1

    override def merge(trees: Trees): Int = trees.map(_.sum).sum

  val solutionB = new Solution:
    override def scoreIfInner(trees: Trees, row: Int, column: Int): Int =
      val rows = Trees.rows(trees)
      val columns = Trees.columns(trees)

      val up = trees.take(row).map(_(column))
      val down = trees.slice(row + 1, rows).map(_(column))
      val left = trees(row).take(column)
      val right = trees(row).slice(column + 1, columns)

      val currentHeight = trees(row)(column)

      val scores = List(up.reverse, down, left.reverse, right)
        .map { treeLine =>
          treeLine.indexWhere(_ >= currentHeight) match
            case -1    => treeLine.length
            case other => other + 1
        }
      val score = scores.fold(1)(_ * _)

      score

    override def scoreIfOuter(trees: Trees): Int = 0

    override def merge(trees: Trees): Int = trees.map(_.max).max

  println(solutionA.solve("./inputs/day8/data.txt"))
  println(solutionB.solve("./inputs/day8/data.txt"))
