class Day3 extends munit.FunSuite:

  // shared code

  def getInput(name: String): List[String] =
    io.Source.fromResource(name)
      .getLines
      .toList

  def score(ss: List[String]): Int = -1

  // part 1

  // part 1 tests

  test("day 3 part 1 sample") {
    assertEquals(0, score(getInput("day3-sample.txt")))
  }

  test("day 3 part 1") {
    assertEquals(0, score(getInput("day3.txt")))
  }

  // part 2

  // part 2 tests

  test("day 3 part 2 sample") {
    assertEquals(0, score(getInput("day3-sample.txt")))
  }

  test("day 3 part 2") {
    assertEquals(0, score(getInput("day3.txt")))
  }

end Day3
