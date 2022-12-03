class Day3 extends munit.FunSuite:

  // shared code

  def getInput(name: String): List[String] =
    io.Source.fromResource(name)
      .getLines
      .toList

  def scoreItem(c: Char): Int =
    if c >= 'a' && c <= 'z' then
      c -'a' + 1
    else if c >= 'A' && c <= 'Z' then
      c -'A' + 27
    else
      throw new IllegalArgumentException(c.toString)

  def score(knapsack: String): Int =
    val (sack1, sack2) = knapsack.splitAt(knapsack.size / 2)
    val commonItems = sack1.intersect(sack2).distinct
    assert(commonItems.size == 1)
    scoreItem(commonItems.head)

  // part 1 tests

  test("day 3 part 1 sample") {
    assertEquals(157, getInput("day3-sample.txt").map(score).sum)
  }

  test("day 3 part 1") {
    assertEquals(7997, getInput("day3.txt").map(score).sum)
  }

end Day3
