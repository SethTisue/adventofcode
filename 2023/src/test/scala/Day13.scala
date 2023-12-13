// Not the greatest code, but it's okay.

class Day13 extends munit.FunSuite:

  /// core logic

  type Grid = Vector[String]

  def mirrorRows(grid: Grid): Seq[Int] =
    grid.indices.filter: rowNumber =>
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

  def solve(grid: Grid, forbiddenAnswer: Int): Int =
    val row = mirrorRows(grid).find(_ != forbiddenAnswer / 100).getOrElse(-1)
    if row != -1
    then row * 100
    else
      mirrorRows(grid.map(_.toVector).transpose.map(_.mkString))
        .find(_ != forbiddenAnswer)
        .getOrElse(-1)

  def part1(name: String): Int =
    getInput(name)
      .map(solve(_, -1))
      .tapEach(n => require(n != -1))
      .sum

  test("part 1 sample"):
    assertEquals(part1("day13-sample.txt"), 405)
  test("part 1"):
    assertEquals(part1("day13.txt"), 42974)

  /// part 2

  import util.chaining.*

  def repair(grid: Grid): Grid =
    val original = solve(grid, -1)
    val candidates: Iterator[Grid] =
      for
        row <- grid.indices.iterator
        col <- grid.head.indices
        flipped =
          if grid(row)(col) == '.'
          then '#'
          else '.'
      yield
        grid.updated(row, grid(row).updated(col, flipped))
    candidates.find: cand =>
      solve(cand, forbiddenAnswer = original) != -1
    .get

  def part2(name: String): Int =
    getInput(name)
      .map: grid =>
         val repaired = repair(grid)
         solve(repaired, solve(grid, -1))
      .tapEach(n => require(n != -1))
      .sum

  test("part 2 sample"):
    assertEquals(part2("day13-sample.txt"), 400)
  test("part 2"):
    assertEquals(part2("day13.txt"), 27587)

end Day13
