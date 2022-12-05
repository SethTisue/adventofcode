class Day04 extends munit.FunSuite:

  // shared code

  def getInput(name: String): List[(Range, Range)] =
    io.Source.fromResource(name)
      .getLines
      .toList
      .map{
        case s"$n1-$n2,$n3-$n4" =>
          (n1.toInt to n2.toInt, n3.toInt to n4.toInt)}

  // part 1

  def isSubsumed(r1: Range, r2: Range) =
    r1.forall(r2.contains) || r2.forall(r1.contains)

  // part 1 tests

  test("day 4 part 1 sample") {
    assertEquals(2, getInput("day04-sample.txt")
      .count(isSubsumed.tupled))
  }

  test("day 4 part 1") {
    assertEquals(580, getInput("day04.txt")
      .count(isSubsumed.tupled))
  }

  // part 2

  def overlaps(r1: Range, r2: Range) =
    r1.intersect(r2).nonEmpty

  // part 2 tests

  test("day 4 part 2 sample") {
    assertEquals(4, getInput("day04-sample.txt")
      .count(overlaps.tupled))
  }

  test("day 4 part 2") {
    assertEquals(895, getInput("day04.txt")
      .count(overlaps.tupled))
  }

end Day04
