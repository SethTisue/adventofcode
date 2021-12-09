class Day1 extends munit.FunSuite:

  val sample = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  // helpers

  def intsFromFile(path: String): List[Int] =
    io.Source.fromFile(path)
      .getLines
      .flatMap(_.toIntOption)
      .toList

  // Part 1

  def increases(xs: List[Int]): Int =
    xs.sliding(2).count {
      case List(prev, cur) =>
        cur > prev
    }

  test("part 1 sample") {
    assertEquals(increases(sample), 7)
  }

  test("part 1 real") {
    assertEquals(increases(intsFromFile("day1.txt")), 1713)
  }

  // Part 2

  def sliding3Increases(xs: List[Int]): Int =
    increases(
      xs.sliding(3).map(_.sum).toList)

  test("part 2 sample") {
    assertEquals(sliding3Increases(sample), 5)
  }

  test("part 2 real") {
    assertEquals(sliding3Increases(intsFromFile("day1.txt")), 1734)
  }

end Day1
