class Day02 extends munit.FunSuite:

  def getInput(name: String): List[List[Int]] =
    io.Source.fromResource(name)
      .getLines
      .map: line =>
        line.split("\\s").map(_.toInt).toList
      .toList

  // part 1

  test("part 1 sample"):
    assertEquals(
      getInput("day02-sample.txt").count(isSafe),
      2)

  test("part 1"):
    assertEquals(
      getInput("day02.txt").count(isSafe),
      218)

  def isSafe(ns: List[Int]): Boolean =
    val diffs = ns.zip(ns.tail).map(_ - _)
    diffs.map(math.signum).distinct.size == 1 &&
      diffs.map(math.abs).forall(d => d >= 1 && d <= 3)

  // part 2

  test("part 2 sample"):
    assertEquals(
      getInput("day02-sample.txt").count(isSafeDampened),
      4)

  test("part 2"):
    assertEquals(
      getInput("day02.txt").count(isSafeDampened),
      290)

  def isSafeDampened(ns: List[Int]): Boolean =
    ns.indices.exists: i =>
      isSafe(ns.patch(i, Nil, 1))

end Day02
