class Day10 extends munit.FunSuite:

  /// data model

  type Position = (Int, Int)  // row, column
  extension (pos: Position)
    inline def row    = pos(0)
    inline def column = pos(1)
    inline def up     = (pos.row - 1, pos.column)
    inline def down   = (pos.row + 1, pos.column)
    inline def left   = (pos.row, pos.column - 1)
    inline def right  = (pos.row, pos.column + 1)

  // out-of-bounds positions are considered empty
  type Grid = Vector[Vector[Char]]
  extension (grid: Grid)
    def at(pos: Position): Char =
      if grid.isDefinedAt(pos.row) && grid.head.isDefinedAt(pos.column)
      then grid(pos.row)(pos.column)
      else '.'

  /// easy logic

  def startingPosition(grid: Grid): Position =
    val rowNumber = grid.indexWhere(_.contains('S'))
    (rowNumber, grid(rowNumber).indexOf('S'))

  // returns the character the 'S' is "concealing" -- the shape
  // of that part of the pipe
  def findConcealed(grid: Grid): Char =
    val start = startingPosition(grid)
    val topOpen    = "|7F".contains(grid.at(start.up))
    val bottomOpen = "|JL".contains(grid.at(start.down))
    val leftOpen   = "-FL".contains(grid.at(start.left))
    val rightOpen  = "-J7".contains(grid.at(start.right))
    (topOpen, bottomOpen, leftOpen, rightOpen) match
      case (true, true, false, false) => '|'
      case (true, false, true, false) => 'J'
      case (true, false, false, true) => 'L'
      case (false, true, true, false) => '7'
      case (false, true, false, true) => 'F'
      case (false, false, true, true) => '-'
      case _ => ??? // impossible if input is well-formed

  def exits(grid: Grid, pos: Position): (Position, Position) =
    grid.at(pos) match
      case '|' => (pos.up, pos.down)
      case '-' => (pos.left, pos.right)
      case 'L' => (pos.up, pos.right)
      case 'J' => (pos.up, pos.left)
      case '7' => (pos.down, pos.left)
      case 'F' => (pos.down, pos.right)
      case 'S' =>
        val cands = Seq(pos.up, pos.down, pos.left, pos.right)
        val Seq(exit1, exit2) =
          cands.filter: next =>
            grid.at(next) != '.' && locally:
              val (ret1, ret2) = exits(grid, next)
              ret1 == pos || ret2 == pos
        (exit1, exit2)

  def nextSquare(grid: Grid, pos: Position, prev: Position): Position =
    val (exit1, exit2) = exits(grid, pos)
    if exit1 == prev
    then exit2
    else exit1

  /// tricky logic

  def findPipe(grid: Grid): Set[Position] =
    val start = startingPosition(grid)
    val (exit1, exit2) = exits(grid, start)
    (Iterator
      .iterate((start, exit1)): (cur, prev) =>
        (nextSquare(grid, cur, prev), cur)
      .map(_(0))
      .drop(1)
      .takeWhile(_ != start)
      .toSet) + start

  def countGrid(grid: Grid, pipe: Set[Position]): Int =
    val pipe = findPipe(grid)
    val concealed = findConcealed(grid)
    def countRow(rowNumber: Int): Int =
      val row = grid(rowNumber)
      var result = 0
      var state = (false, false)  // top half, bottom half
      for columnNumber <- row.indices do
        val pos = (rowNumber, columnNumber)
        val cell =
          val value = grid.at(pos)
          if value == 'S'
          then concealed
          else if pipe(pos)
          then value
          else '.'
        val next =
          cell match
            case '.' | '-' => (state(0),  state(0), state(1),  state(1))  // flow, flow
            case '|'       => (state(0), !state(0), state(1), !state(1))  // flip, flip
            case 'F' | '7' => (state(0),  state(0), state(1), !state(1))  // flow, flip
            case 'L' | 'J' => (state(0), !state(0), state(1),  state(1))  // flip, flow
        if next == (true, true, true, true) then
          result += 1
        state = (next(1), next(3))
      result
    grid.indices.map(countRow).sum

  /// reading & parsing

  def getInput(name: String): Grid =
    io.Source.fromResource(name)
      .getLines
      .map(_.toVector)
      .toVector

  /// part 1

  def part1(name: String): Int =
    val grid = getInput(name)
    findPipe(grid).size / 2

  test("part 1 sample"):
    assertEquals(part1("day10-sample.txt"), 8)
  test("part 1"):
    assertEquals(part1("day10.txt"), 6828)

  /// part 2

  def part2(name: String): Int =
    val input = getInput(name)
    countGrid(input, findPipe(input))

  test("part 2 sample"):
    assertEquals(part2("day10-sample.txt"), 1)
  test("part 2 sample 2a"):
    assertEquals(part2("day10-sample2a.txt"), 4)
  test("part 2 sample 2b"):
    assertEquals(part2("day10-sample2b.txt"), 4)
  test("part 2 sample 3"):
    assertEquals(part2("day10-sample3.txt"), 8)
  test("part 2 sample 4"):
    assertEquals(part2("day10-sample4.txt"), 10)
  test("part 2"):
    assertEquals(part2("day10.txt"), 459)

end Day10
