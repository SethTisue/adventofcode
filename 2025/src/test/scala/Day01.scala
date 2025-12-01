class Day01 extends munit.FunSuite:

  /// reading & parsing

  def getInput(name: String): Vector[Int] =
    io.Source.fromResource(name)
      .getLines
      .map: line =>
        val sign = if line.head == 'L' then -1 else 1
        line.tail.toInt * sign
      .toVector

  /// part 1

  def part1(name: String): Int =
    getInput(name)
      .scanLeft(50): (pos, turn) =>
        (pos + turn + 100) % 100
      .count(_ == 0)

  test("part 1 sample"):
    assertEquals(part1("day01-sample.txt"), 3)
  test("part 1"):
    assertEquals(part1("day01.txt"), 989)

/*
  /// part 2

  def part2(name: String): Int =
    getInput(name).size

  test("part 2 sample"):
    assertEquals(part2("day01-sample.txt"), 0)
  test("part 2"):
    assertEquals(part2("day01.txt"), 0)
*/

end Day01

