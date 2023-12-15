import annotation.tailrec

class Day15 extends munit.FunSuite:

  /// core logic

  @tailrec
  final def hash(s: String, accum: Int = 0): Int =
    if s.isEmpty
    then accum
    else
      hash(s.tail, (accum + s.head) * 17 % 256)

  /// reading & parsing

  def getInput(name: String): Seq[String] =
    io.Source.fromResource(name)
      .getLines
      .flatMap(_.split(','))
      .toVector

  /// part 1

  def part1(name: String): Int =
    getInput(name).map(hash(_)).sum

  test("part 1 easy sample"):
    assertEquals(hash("HASH"), 52)
  test("part 1 sample"):
    assertEquals(part1("day15-sample.txt"), 1320)
/*
  test("part 1"):
    assertEquals(part1("day15.txt"), 0)

  /// part 2

  def part2(name: String): Int =
    getInput(name).size

  test("part 2 sample"):
    assertEquals(part2("day15-sample.txt"), 0)
  test("part 2"):
    assertEquals(part2("day15.txt"), 0)
*/

end Day15
