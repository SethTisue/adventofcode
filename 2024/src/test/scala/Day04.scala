class Day04 extends munit.FunSuite:

  /// reading & parsing

  def getInput(name: String): Vector[String] =
    io.Source.fromResource(name)
      .getLines
      .toVector

  /// part 1

  def part1(name: String): Int =
    val input = getInput(name)
    val trans = input.map(_.toVector).transpose.map(_.mkString)
    val dim = input.head.size
    val diags1 =
      for i <- (-dim to dim).toVector
      yield
        (for (row, j) <- input.zipWithIndex
        yield row.lift(i + j)).flatten.mkString
    val diags2 =
      for i <- (-dim to dim).toVector
      yield
        (for (row, j) <- input.zipWithIndex
        yield row.lift(dim - j + i)).flatten.mkString
    def occur(s: String): Int =
      "XMAS".r.findAllIn(s).size + "SAMX".r.findAllIn(s).size
    def count(ss: Vector[String]) = ss.map(occur).sum
    count(input) + count(trans) + count(diags1) + count(diags2)

  test("part 1 sample"):
    assertEquals(part1("day04-sample.txt"), 18)
  test("part 1"):
    assertEquals(part1("day04.txt"), 2524)

  /// part 2

  def part2(name: String): Int =
    val input = getInput(name)
    val coords =
      for
        i <- input.indices.drop(1).dropRight(1)
        j <- input.indices.drop(1).dropRight(1)
      yield (i, j)
    val crosses = Set("AMMSS", "ASSMM", "ASMSM", "AMSMS")
    coords.count: (i, j) =>
      crosses(
        List(
         input(i)(j),
         input(i - 1)(j - 1),
         input(i - 1)(j + 1),
         input(i + 1)(j - 1),
         input(i + 1)(j + 1)).mkString)

  test("part 2 sample"):
    assertEquals(part2("day04-sample.txt"), 9)
  test("part 2"):
    assertEquals(part2("day04.txt"), 1873)

end Day04

