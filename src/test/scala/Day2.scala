class Day2 extends munit.FunSuite:

  enum Play:
    case Rock, Paper, Scissors

  case class Round(play: Play, response: Play)
  object Round:
    def fromChar(c: Char): Play = c match
      case 'A' | 'X' => Play.Rock
      case 'B' | 'Y' => Play.Paper
      case 'C' | 'Z' => Play.Scissors

  def getInput(name: String): List[Round] =
    io.Source.fromResource(name)
      .getLines
      .map{case s"$play $response" =>
        Round(Round.fromChar(play(0)), Round.fromChar(response(0)))
      }
      .toList

  // part 1

  def score(rounds: List[Round]): Int =
    0

  test("day 2 part 1 sample") {
    assertEquals(15, score(getInput("day2-sample.txt")))
  }

  test("day 2 part 1") {
    assertEquals(71471, score(getInput("day1.txt")))
  }

end Day2
