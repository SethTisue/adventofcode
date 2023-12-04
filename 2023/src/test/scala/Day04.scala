class Day04 extends munit.FunSuite:

  /// data model

  /// reading and parsing

  def getInput(name: String): Vector[String] =
    io.Source.fromResource(name)
      .getLines.toVector

  /// part 1

  def part1(name: String): Int =
    val input = getInput(name)
    0

  test("part 1 sample"):
    assertEquals(0, part1("day04-sample.txt"))
  test("part 1"):
    assertEquals(0, part1("day04.txt"))

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
