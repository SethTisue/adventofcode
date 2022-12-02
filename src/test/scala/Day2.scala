class Day2 extends munit.FunSuite:

  enum Play:
    case Rock, Paper, Scissors
  object Play:
    def fromChar(c: Char): Play = c match
      case 'A' | 'X' => Play.Rock
      case 'B' | 'Y' => Play.Paper
      case 'C' | 'Z' => Play.Scissors

  case class Part1Round(play: Play, response: Play)
  def getInputPart1(name: String): List[Part1Round] =
    io.Source.fromResource(name)
      .getLines
      .map{case s"$play $response" =>
        Part1Round(
          Play.fromChar(play(0)),
          Play.fromChar(response(0)))
      }
      .toList

  // part 1

  def scorePart1(rounds: List[Part1Round]): Int =
    def playScore(p: Play): Int = p match
      case Play.Rock     => 1
      case Play.Paper    => 2
      case Play.Scissors => 3
    def roundScore(p1: Play, p2: Play): Int = (p1, p2) match
      case (Play.Rock, Play.Paper)     => 6
      case (Play.Paper, Play.Scissors) => 6
      case (Play.Scissors, Play.Rock)  => 6
      case _ if p1 == p2               => 3
      case _                           => 0
    rounds.map{case Part1Round(play, response) =>
      playScore(response) + roundScore(play, response)
    }.sum

  test("day 2 part 1 sample") {
    assertEquals(15, scorePart1(getInputPart1("day2-sample.txt")))
  }

  test("day 2 part 1") {
    assertEquals(15572, scorePart1(getInputPart1("day2.txt")))
  }

  // part 2

  enum Outcome:
    case Lose, Draw, Win
  object Outcome:
    def fromChar(c: Char): Outcome = c match
      case 'X' => Outcome.Lose
      case 'Y' => Outcome.Draw
      case 'Z' => Outcome.Win

  case class Part2Round(play: Play, outcome: Outcome)
  def getInputPart2(name: String): List[Part2Round] =
    io.Source.fromResource(name)
      .getLines
      .map{case s"$play $outcome" =>
        Part2Round(
          Play.fromChar(play(0)),
          Outcome.fromChar(outcome(0)))}
      .toList

  def scorePart2(rounds: List[Part2Round]): Int =
    def playScore(p: Play): Int = p match
      case Play.Rock     => 1
      case Play.Paper    => 2
      case Play.Scissors => 3
    def roundScore(p: Play, o: Outcome): Int = (p, o) match
      case _                           => 0
    def outcomeScore(o: Outcome): Int = o match
      case Outcome.Lose => 0
      case Outcome.Draw => 3
      case Outcome.Win  => 6
    def chooseResponse(p: Play, o: Outcome): Play = (p, o) match
      case (_, Outcome.Draw) => p
      case (Play.Rock, Outcome.Win) => Play.Paper
      case (Play.Rock, Outcome.Lose) => Play.Scissors
      case (Play.Paper, Outcome.Win) => Play.Scissors
      case (Play.Paper, Outcome.Lose) => Play.Rock
      case (Play.Scissors, Outcome.Win) => Play.Rock
      case (Play.Scissors, Outcome.Lose) => Play.Paper

    rounds.map{
      case Part2Round(play, outcome) =>
        val response = chooseResponse(play, outcome)
        playScore(response) + outcomeScore(outcome)
    }.sum

  test("day 2 part 1 sample") {
    assertEquals(12, scorePart2(getInputPart2("day2-sample.txt")))
  }

  test("day 2 part 1") {
    assertEquals(16098, scorePart2(getInputPart2("day2.txt")))
  }

end Day2
