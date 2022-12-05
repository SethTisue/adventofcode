class Day02 extends munit.FunSuite:

  // shared code

  enum Play:
    case Rock, Paper, Scissors
  object Play:
    def fromChar(c: Char): Play = c match
      case 'A' | 'X' => Play.Rock
      case 'B' | 'Y' => Play.Paper
      case 'C' | 'Z' => Play.Scissors
    def score(p: Play): Int = p match
      case Play.Rock     => 1
      case Play.Paper    => 2
      case Play.Scissors => 3
    def beats(p: Play): Play =
      Play.fromOrdinal((p.ordinal + 1) % Play.values.size)
    def losesTo(p: Play): Play =
      Play.fromOrdinal((p.ordinal + Play.values.size - 1) % Play.values.size)

  enum Outcome:
    case Lose, Draw, Win
  object Outcome:
    def fromChar(c: Char): Outcome = c match
      case 'X' => Outcome.Lose
      case 'Y' => Outcome.Draw
      case 'Z' => Outcome.Win
    def score(o: Outcome): Int = o match
      case Outcome.Lose => 0
      case Outcome.Draw => 3
      case Outcome.Win  => 6

  // part 1

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

  def scorePart1(rounds: List[Part1Round]): Int =
    def outcome(p1: Play, p2: Play): Outcome =
      if p1 == p2 then
        Outcome.Draw
      else if p2 == Play.beats(p1) then
        Outcome.Win
      else
        Outcome.Lose
    rounds.map{case Part1Round(play, response) =>
      Play.score(response) + Outcome.score(outcome(play, response))
    }.sum

  // part 1 tests

  test("day 2 part 1 sample") {
    assertEquals(15, scorePart1(getInputPart1("day02-sample.txt")))
  }

  test("day 2 part 1") {
    assertEquals(15572, scorePart1(getInputPart1("day02.txt")))
  }

  // part 2

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
    def chooseResponse(p: Play, o: Outcome): Play = o match
      case Outcome.Draw => p
      case Outcome.Win => Play.beats(p)
      case Outcome.Lose => Play.losesTo(p)
    rounds.map{
      case Part2Round(p, o) =>
        Play.score(chooseResponse(p, o)) + Outcome.score(o)
    }.sum

  // part 2 tests

  test("day 2 part 2 sample") {
    assertEquals(12, scorePart2(getInputPart2("day02-sample.txt")))
  }

  test("day 2 part 2") {
    assertEquals(16098, scorePart2(getInputPart2("day02.txt")))
  }

end Day02
