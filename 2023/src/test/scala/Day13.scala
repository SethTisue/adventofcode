class Day13 extends munit.FunSuite:

  /// core logic

  type Grid = Vector[String]

  def mirrorRow(grid: Grid): Int =
    grid.indices.indexWhere: rowNumber =>
      val rowsAbove = grid.take(rowNumber)
      val rowsBelow = grid.drop(rowNumber)
      val maxDim = rowsAbove.size.min(rowsBelow.size)
      maxDim > 0 &&
        (rowsAbove.takeRight(maxDim) == rowsBelow.take(maxDim).reverse)

  /// reading & parsing

  def getInput(name: String): Seq[Grid] =
    io.Source.fromResource(name)
      .mkString
      .split("\n\n")
      .map: section =>
        section.linesIterator.toVector
      .toSeq

  /// part 1

  def part1(name: String): Int =
    getInput(name)
      .map: grid =>
        val row = mirrorRow(grid)
        if row != -1
        then 100 * row
        else
          mirrorRow(grid.map(_.toVector).transpose.map(_.mkString))
            .ensuring(_ != -1, grid)
      .sum

  test("part 1 sample"):
    assertEquals(part1("day13-sample.txt"), 405)
  test("part 1"):
    assertEquals(part1("day13.txt"), 42974)

/*
  /// part 2

  def part2(name: String): Int =
    getInput(name).size

  test("part 2 sample"):
    assertEquals(part2("day13-sample.txt"), 0)
  test("part 2"):
    assertEquals(part2("day13.txt"), 0)
*/

end Day13
