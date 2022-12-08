class Day08 extends munit.FunSuite:

  type Forest = Vector[Vector[Int]]

  def getInput(name: String): Forest =
    io.Source.fromResource(name)
      .getLines
      .map(_.map(_.toString.toInt).toVector)
      .toVector

  def isVisible(x: Int, y: Int, forest: Forest): Boolean =
    val height = forest(x)(y)
    (0 to (x - 1)).forall(x0 => forest(x0)(y) < height) ||
      ((x + 1) until forest.size).forall(x0 => forest(x0)(y) < height) ||
      (0 to (y - 1)).forall(y0 => forest(x)(y0) < height) ||
      ((y + 1) until forest.size).forall(y0 => forest(x)(y0) < height)

  def visibleCount(forest: Forest): Int =
    val allTrees: Vector[(Int, Int)] =
      for x <- forest.indices.toVector
          y <- forest(x).indices
      yield (x, y)
    allTrees.count((x, y) => isVisible(x, y, forest))

  // part 1 tests

  test("day 8 part 1 sample") {
    val input = getInput("day08-sample.txt")
    assertEquals(21, visibleCount(input))
  }

  test("day 8 part 1") {
    val input = getInput("day08.txt")
    assertEquals(1776, visibleCount(input))
  }

end Day08
