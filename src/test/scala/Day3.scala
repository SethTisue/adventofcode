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

  // part 1

  def scorePart1(knapsack: String): Int =
    val (sack1, sack2) = knapsack.splitAt(knapsack.size / 2)
    val commonItems = sack1.intersect(sack2).distinct
    assert(commonItems.size == 1)
    scoreItem(commonItems.head)

  // part 1 tests

  test("day 3 part 1 sample") {
    assertEquals(157, getInput("day3-sample.txt").map(scorePart1).sum)
  }

  test("day 3 part 1") {
    assertEquals(7997, getInput("day3.txt").map(scorePart1).sum)
  }

  // part 2

  case class Group(elves: List[String])

  def getInputPart2(name: String): List[Group] =
    io.Source.fromResource(name)
      .getLines
      .grouped(3)
      .map(knapsacks => Group(knapsacks.toList))
      .toList

  def scorePart2(group: Group): Int =
    scoreItem(group.elves.reduceLeft(_.intersect(_)).head)

  // part 2 tests

  test("day 3 part 2 sample") {
    assertEquals(70, getInputPart2("day3-sample.txt").map(scorePart2).sum)
  }

  test("day 3 part 2") {
    assertEquals(2545, getInputPart2("day3.txt").map(scorePart2).sum)
  }

end Day3
