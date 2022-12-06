class Day06 extends munit.FunSuite:

  def getInput(name: String): Iterator[String] =
    io.Source.fromResource(name)
      .getLines

  def solve(message: String, window: Int): Int =
   message
    .sliding(window)
    .zipWithIndex
    .collectFirst{
      case (s, i) if s.distinct.size == window =>
        i + window}
    .get

  // part 1 tests

  test("day 6 part 1 sample") {
    val lines = getInput("day06-sample.txt")
    assertEquals(7, solve(lines.next(), 4))
    assertEquals(5, solve(lines.next(), 4))
    assertEquals(6, solve(lines.next(), 4))
    assertEquals(10, solve(lines.next(), 4))
    assertEquals(11, solve(lines.next(), 4))
  }

  test("day 6 part 1") {
    val lines = getInput("day06.txt")
    assertEquals(1275, solve(lines.next(), 4))
  }

  // part 2 tests

  test("day 6 part 2 sample") {
    val lines = getInput("day06-sample.txt")
    assertEquals(19, solve(lines.next(), 14))
    assertEquals(23, solve(lines.next(), 14))
    assertEquals(23, solve(lines.next(), 14))
    assertEquals(29, solve(lines.next(), 14))
    assertEquals(26, solve(lines.next(), 14))
  }

  test("day 6 part 2") {
    val lines = getInput("day06.txt")
    assertEquals(3605, solve(lines.next(), 14))
  }

end Day06
