class Day11 extends munit.FunSuite:

  /// core logic

  /// reading & parsing

  def getInput(name: String): Vector[String] =
    io.Source.fromResource(name)
      .getLines
      .toVector

  /// part 1

  def part1(name: String): Int =
    getInput(name).size

  test("part 1 sample"):
    assertEquals(part1("day11-sample.txt"), 0)
  test("part 1"):
    assertEquals(part1("day11.txt"), 0)

  /// part 2

  def part2(name: String): Int =
    getInput(name).size

  test("part 2 sample"):
    assertEquals(part2("day11-sample.txt"), 0)
  test("part 2"):
    assertEquals(part2("day11.txt"), 0)

end Day11
