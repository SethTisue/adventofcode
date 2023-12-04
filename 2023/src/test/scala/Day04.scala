class Day04 extends munit.FunSuite:

  /// data model

  case class Card(winners: Set[Int], played: Set[Int])

  /// reading and parsing

  def getInput(name: String): Vector[Card] =
    def fromNumberString(s: String): Set[Int] =
      s.split(' ').filter(_.nonEmpty).map(_.toInt).toSet
    io.Source.fromResource(name)
      .getLines.toVector
      .map:
        case s"Card $_: $xs1 | $xs2" =>
          Card(fromNumberString(xs1), fromNumberString(xs2))

  /// part 1

  def part1(name: String): Int =
    getInput(name).map: card =>
      Math.pow(2, card.played.intersect(card.winners).size).toInt / 2
    .sum

  test("part 1 sample"):
    assertEquals(part1("day04-sample.txt"), 13)
  test("part 1"):
    assertEquals(part1("day04.txt"), 24175)

/*
  /// part 2

  def part2(name: String): Int =
    val input = getInput(name)
    0

  test("part 2 sample"):
    assertEquals(0, part2("day04-sample.txt"))
  test("part 2"):
    assertEquals(0, part2("day04.txt"))
*/

end Day04
