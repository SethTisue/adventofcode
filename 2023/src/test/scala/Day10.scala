class Day10 extends munit.FunSuite:

  /// core logic

  type Grid = Vector[Vector[Char]]
  type Position = (Int, Int)  // row, column
  extension (pos: Position)
    def row = pos(0)
    def column = pos(1)
    def up    = (pos(0) - 1, pos(1))
    def down  = (pos(0) + 1, pos(1))
    def left  = (pos(0), pos(1) - 1)
    def right = (pos(0), pos(1) + 1)
  extension (grid: Grid)
    def at(pos: Position): Char =
      if grid.indices.contains(pos.row) && grid.head.indices.contains(pos.column)
      then grid(pos.row)(pos.column)
      else '.'

  def startingPosition(grid: Grid): Position =
    val row = grid.indexWhere(_.contains('S'))
    (row, grid(row).indexOf('S'))

  def exits(grid: Grid, cur: Position): (Position, Position) =
    grid.at(cur) match
      case '|' => (cur.up, cur.down)
      case '-' => (cur.left, cur.right)
      case 'L' => (cur.up, cur.right)
      case 'J' => (cur.up, cur.left)
      case '7' => (cur.down, cur.left)
      case 'F' => (cur.down, cur.right)
      case 'S' =>
        val cands = Seq(cur.up, cur.down, cur.left, cur.right)
        val Seq(exit1, exit2) =
          cands.filter: next =>
            grid.at(next) != '.' && locally:
              val (ret1, ret2) = exits(grid, next)
              ret1 == cur || ret2 == cur
        (exit1, exit2)

  def nextSquare(grid: Grid, cur: Position, prev: Position): Position =
    val (exit1, exit2) = exits(grid, cur)
    if exit1 == prev
    then exit2
    else exit1

  /// reading & parsing

  def getInput(name: String): Grid =
    io.Source.fromResource(name)
      .getLines
      .map(_.toVector)
      .toVector

  /// part 1

  def part1(name: String): Int =
    val grid = getInput(name)
    val start = startingPosition(grid)
    val (exit1, exit2) = exits(grid, start)
    val pipe = LazyList.iterate((start, exit1)): (cur, prev) =>
      (nextSquare(grid, cur, prev), cur)
    pipe.map(_(0)).map(grid.at).tail.takeWhile(_ != 'S').size / 2 + 1

  test("part 1 sample"):
    assertEquals(part1("day10-sample.txt"), 8)
  test("part 1"):
    assertEquals(part1("day10.txt"), 6828)

  /// part 2

/*
  def part2(name: String): Int =
    getInput(name).size

  test("part 2 sample"):
    assertEquals(part2("day10-sample.txt"), 2)
  test("part 2"):
    assertEquals(part2("day10.txt"), 908)

end Day10
*/
