class Day11 extends munit.FunSuite:

  /// core logic

  type Position = (Long, Long)
  def manhattanDistance(p1: Position, p2: Position): Long =
    math.abs(p1(0) - p2(0)) + math.abs(p1(1) - p2(1))
  type Universe = Seq[Position]

  /// reading & parsing

  def getInput(name: String): Universe =
    val lines =
      io.Source.fromResource(name)
        .getLines
        .toVector
    for
      (row, rowNumber) <- lines.zipWithIndex
      (cell, columnNumber) <- row.zipWithIndex
      if cell == '#'
    yield (rowNumber, columnNumber)

  /// part 1

  def expand(universe: Universe, factor: Long): Universe =
    val maxRow = universe.map(_(0)).max
    val emptyRows =
      0L.to(maxRow).filterNot(r => universe.exists(_(0) == r))
    val maxCol = universe.map(_(1)).max
    val emptyCols =
      0L.to(maxCol).filterNot(c => universe.exists(_(1) == c))
    for (row, col) <- universe
    yield (row + (factor - 1) * (emptyRows.count(_ < row)),
           col + (factor - 1) * (emptyCols.count(_ < col)))

  def allDistancesSum(name: String, factor: Long): Long =
    val universe = expand(getInput(name), factor)
    (for case Seq(p1, p2) <- universe.combinations(2)
     yield manhattanDistance(p1, p2)).sum

  test("part 1 sample"):
    assertEquals(allDistancesSum("day11-sample.txt", 2), 374L)
  test("part 1"):
    assertEquals(allDistancesSum("day11.txt", 2), 9556712L)

  /// part 2

  test("part 2 sample 1"):
    assertEquals(allDistancesSum("day11-sample.txt", 10), 1030L)
  test("part 2 sample 2"):
    assertEquals(allDistancesSum("day11-sample.txt", 100), 8410L)
  test("part 2"):
    assertEquals(allDistancesSum("day11.txt", 1_000_000L), 678626199476L)

end Day11
