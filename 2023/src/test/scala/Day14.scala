class Day14 extends munit.FunSuite:

  /// core logic

  def rollOnce(s: String): String =
    var s2 = s
    for i <- s.indices.init
    do
      if s2(i) == 'O' && s2(i + 1) == '.'
      then s2 = s2.updated(i, '.').updated(i + 1, 'O')
    s2

  def roll(s: String): String =
    val result = rollOnce(s)
    if s == result
    then s
    else roll(result)

  def score(s: String): Int =
    (for
      (c, n) <- s.zipWithIndex
      if c == 'O'
    yield n + 1).sum

  /// reading & parsing

  // each column is independent, so we `transpose` to get
  // columns instead of rows. and `reverse` so the load
  // will increase with increasing index

  def getInput(name: String): Vector[String] =
    io.Source.fromResource(name)
      .getLines
      .map(_.toVector)
      .toVector
      .transpose
      .map(_.mkString.reverse)

  /// part 1

  def part1(name: String): Int =
    getInput(name)
      .map(roll)
      .map(score)
      .sum

  test("part 1 sample"):
    assertEquals(part1("day14-sample.txt"), 136)
  test("part 1"):
    assertEquals(part1("day14.txt"), 105208)

/*
  /// part 2

  def part2(name: String): Int =
    getInput(name).size

  test("part 2 sample"):
    assertEquals(part2("day14-sample.txt"), 0)
  test("part 2"):
    assertEquals(part2("day14.txt"), 0)
*/

end Day14
