class Day13 extends munit.FunSuite:

  /// core logic

  type Grid = Vector[Vector[Boolean]]

  def mirrorRow(grid: Grid): Int =
    grid.indices.indexWhere: rowNumber =>
      val rowsAbove = grid.take(rowNumber)
      val rowsBelow = grid.drop(rowNumber)
      val maxDim = rowsAbove.size.min(rowsBelow.size)
      maxDim > 0 &&
        (rowsAbove.takeRight(maxDim) == rowsBelow.take(maxDim))

  /// reading & parsing

  def getInput(name: String): Seq[Grid] =
    io.Source.fromResource(name)
      .mkString
      .split("\n\n")
      .map: section =>
        section.linesIterator.map: line =>
          line.map(_ == '#').toVector
        .toVector
      .toSeq

  /// part 1

  def part1(name: String): Int =
    getInput(name)
      .map: grid =>
        val row = mirrorRow(grid)
        if row != -1
        then 100 * row
        else
          mirrorRow(grid.transpose)
            .ensuring(_ != -1, grid)
      .tapEach(println)
      .sum

  test("part 1 sample"):
    assertEquals(part1("day13-sample.txt"), 405)
  test("part 1"):
    assertEquals(part1("day13.txt"), 0)

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

