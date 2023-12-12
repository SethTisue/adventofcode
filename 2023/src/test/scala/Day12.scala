class Day12 extends munit.FunSuite:

  /// core logic

  /// reading & parsing

  def getInput(name: String): Vector[(Vector[Char], Vector[Int])] =
    def parse(line: String) =
      line.split(' ') match
        case Array(states, counts) =>
          (states.toVector,
            counts.split(',').map(_.toInt).toVector)
    io.Source.fromResource(name)
      .getLines
      .map(parse)
      .toVector

  /// part 1

  def part1(name: String): Int =
    getInput(name).size

  test("part 1 sample"):
    assertEquals(part1("day12-sample.txt"), 21)
/*
  test("part 1"):
    assertEquals(part1("day12.txt"), 0)

  /// part 2

  def part2(name: String): Int =
    getInput(name).size

  test("part 2 sample"):
    assertEquals(part2("day12-sample.txt"), 0)
  test("part 2"):
    assertEquals(part2("day12.txt"), 0)
*/

end Day12
