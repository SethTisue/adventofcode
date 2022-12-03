class Day3 extends munit.FunSuite:

  // shared code

  type Item = Char
  type Knapsack = String

  def getInput(name: String): List[Knapsack] =
    io.Source.fromResource(name)
      .getLines
      .toList

  def scoreItem(i: Item): Int =
    if i >= 'a' && i <= 'z' then
      i -'a' + 1
    else if i >= 'A' && i <= 'Z' then
      i -'A' + 27
    else
      throw new IllegalArgumentException(i.toString)

  def scoreGroup(sacks: List[Knapsack]): Int =
    scoreItem(sacks.reduceLeft(_.intersect(_)).head)

  // part 1

  def scorePart1(knapsack: String): Int =
    val (sack1, sack2) = knapsack.splitAt(knapsack.size / 2)
    scoreGroup(List(sack1, sack2))

  // part 1 tests

  test("day 3 part 1 sample") {
    assertEquals(157, getInput("day3-sample.txt").map(scorePart1).sum)
  }

  test("day 3 part 1") {
    assertEquals(7997, getInput("day3.txt").map(scorePart1).sum)
  }

  // part 2

  def getInputPart2(name: String): List[List[Knapsack]] =
    getInput(name)
      .grouped(3)
      .toList

  // part 2 tests

  test("day 3 part 2 sample") {
    assertEquals(70, getInputPart2("day3-sample.txt").map(scoreGroup).sum)
  }

  test("day 3 part 2") {
    assertEquals(2545, getInputPart2("day3.txt").map(scoreGroup).sum)
  }

end Day3
