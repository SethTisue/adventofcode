class Day1 extends munit.FunSuite:

  case class Elf(meals: List[Int])

  def getInput(name: String): List[Elf] =
    val result = collection.mutable.ListBuffer.empty[Elf]
    var remaining = io.Source.fromResource(name).getLines
    while remaining.hasNext do
      val (elf, rest) = remaining.span(_.nonEmpty)
      result += Elf(elf.map(_.toInt).toList)
      if rest.hasNext then rest.next()
      remaining = rest
    result.toList

  def mostTotalCalories(elves: List[Elf]): Int =
    elves.map(_.meals.sum).max

  test("day 1 sample") {
    assertEquals(24000, mostTotalCalories(getInput("day1-sample.txt")))
  }

  test("day 1") {
    assertEquals(71471, mostTotalCalories(getInput("day1.txt")))
  }
