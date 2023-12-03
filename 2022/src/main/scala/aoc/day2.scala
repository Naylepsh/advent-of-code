package aoc

object day2 extends App:
  enum Shape:
    case Rock, Paper, Scissors

  def solve[A](
      parseRow: String => (Shape, A),
      calculateResult: (Shape, A) => (Shape, GameResult)
  )(
      inputPath: String
  ): Int =
    getFileContent(inputPath).toList
      .map(parseRow)
      .map { case (opponent, a) =>
        val (player, gameResult) = calculateResult(opponent, a)
        resultScore(gameResult) + shapeScore(player)
      }
      .sum

  def asOpponentShape(str: String): Shape = str match
    case "A" => Shape.Rock
    case "B" => Shape.Paper
    case "C" => Shape.Scissors

  def shapeScore(shape: Shape): Int = shape match
    case Shape.Rock     => 1
    case Shape.Paper    => 2
    case Shape.Scissors => 3

  enum GameResult:
    case Win, Lose, Draw

  def resultScore(result: GameResult): Int = result match
    case GameResult.Win  => 6
    case GameResult.Draw => 3
    case GameResult.Lose => 0

  object init:
    def parseRow(str: String): (Shape, Shape) =
      str.split(" ").toList match
        case opponent :: player :: Nil =>
          (asOpponentShape(opponent), asPlayerShape(player))

    def calculateResult(
        opponent: Shape,
        player: Shape
    ): (Shape, GameResult) =
      val result = (opponent, player) match
        case (Shape.Rock, Shape.Paper)     => GameResult.Win
        case (Shape.Rock, Shape.Scissors)  => GameResult.Lose
        case (Shape.Paper, Shape.Rock)     => GameResult.Lose
        case (Shape.Paper, Shape.Scissors) => GameResult.Win
        case (Shape.Scissors, Shape.Rock)  => GameResult.Win
        case (Shape.Scissors, Shape.Paper) => GameResult.Lose
        case (_, _)                        => GameResult.Draw
      (player, result)

    private def asPlayerShape(str: String): Shape = str match
      case "X" => Shape.Rock
      case "Y" => Shape.Paper
      case "Z" => Shape.Scissors

  object actual:
    def parseRow(str: String): (Shape, GameResult) =
      str.split(" ").toList match
        case opponent :: gameResult :: Nil =>
          (asOpponentShape(opponent), asGameResult(gameResult))

    def calculateResult(
        opponent: Shape,
        result: GameResult
    ): (Shape, GameResult) =
      val player = (opponent, result) match
        case (shape, GameResult.Draw)          => shape
        case (Shape.Rock, GameResult.Lose)     => Shape.Scissors
        case (Shape.Rock, GameResult.Win)      => Shape.Paper
        case (Shape.Paper, GameResult.Lose)    => Shape.Rock
        case (Shape.Paper, GameResult.Win)     => Shape.Scissors
        case (Shape.Scissors, GameResult.Lose) => Shape.Paper
        case (Shape.Scissors, GameResult.Win)  => Shape.Rock

      (player, result)

    private def asGameResult(str: String): GameResult = str match
      case "X" => GameResult.Lose
      case "Y" => GameResult.Draw
      case "Z" => GameResult.Win

  val solveInit = solve(init.parseRow, init.calculateResult)
  val solveActual = solve(actual.parseRow, actual.calculateResult)

  // val res = solveInit("./inputs/day2/data.txt")
  val res = solveActual("./inputs/day2/data.txt")
  println(res)
