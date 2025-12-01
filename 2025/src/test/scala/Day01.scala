class Day01 extends munit.FunSuite:

  /// core logic

  enum Direction:
    case Left, Right
  case class Turn(direction: Direction, distance: Int)
  object Turn:
    def parse(s: String): Turn =
      Turn(
        s.head match
          case 'L' => Direction.Left
          case 'R' => Direction.Right
        , s.tail.toInt)

  /// reading & parsing

  def getInput(name: String): Vector[Turn] =
    io.Source.fromResource(name)
      .getLines
      .map(Turn.parse)
      .toVector

  /// part 1

  def part1(name: String): Int =
    ???

  test("part 1 sample"):
    assertEquals(part1("day01-sample.txt"), 3)
/*
  test("part 1"):
    assertEquals(part1("day01.txt"), 0)

  /// part 2

  def part2(name: String): Int =
    getInput(name).size

  test("part 2 sample"):
    assertEquals(part2("day01-sample.txt"), 0)
  test("part 2"):
    assertEquals(part2("day01.txt"), 0)
*/

end Day01

