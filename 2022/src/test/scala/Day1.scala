class Day1 extends munit.FunSuite:

  case class Elf(meals: List[Int])

  def getInput(name: String): List[Elf] =
    io.Source.fromResource(name)
      .mkString.split("\n\n").toList
      .map(section =>
        Elf(section.split('\n').map(_.toInt).toList))

  // part 1

  def mostTotalCalories(elves: List[Elf]): Int =
    elves.map(_.meals.sum).max

  test("day 1 part 1 sample") {
    assertEquals(24000, mostTotalCalories(getInput("day1-sample.txt")))
  }

  test("day 1 part 1") {
    assertEquals(71471, mostTotalCalories(getInput("day1.txt")))
  }

  // part 2

  def topThreeTotalCalories(elves: List[Elf]): Int =
    elves.map(_.meals.sum).sorted.takeRight(3).sum

  test("day 1 part 2 sample") {
    assertEquals(45000, topThreeTotalCalories(getInput("day1-sample.txt")))
  }

  test("day 1 part 2") {
    assertEquals(211189, topThreeTotalCalories(getInput("day1.txt")))
  }

end Day1
