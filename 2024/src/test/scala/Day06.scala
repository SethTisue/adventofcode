class Day06 extends munit.FunSuite:

  type Position = (Int, Int)
  type Obstacles = Set[Position]
  case class Board(size: Int, initial: Position, obstacles: Obstacles)
  type Heading = (Int, Int)
  def turnRight(heading: Heading): Heading =
    heading match
      case(-1, 0) => (0, 1)
      case(1, 0) => (0, -1)
      case(0, 1) => (1, 0)
      case(0, -1) => (-1, 0)
  enum Outcome:
    case Loop
    case Exit(visited: Int)

  def navigate(board: Board): Outcome =
    case class State(position: Position, heading: Heading)
    var state = State(board.initial, (-1, 0))
    val visited = collection.mutable.Set[State](state)
    def next: Position = (state.position._1 + state.heading._1, state.position._2 + state.heading._2)
    def inBounds(pos: Position) =
      pos._1 >= 0 && pos._1 < board.size && pos._2 >= 0 && pos._2 < board.size
    while inBounds(next) do
      while board.obstacles.contains(next) do
        state = state.copy(heading = turnRight(state.heading))
      state = state.copy(position = next)
      if visited(state)
      then return Outcome.Loop
      visited += state
    Outcome.Exit(visited.map(_.position).size)

  /// reading & parsing

  def getInput(name: String): Board =
    val lines =
      io.Source.fromResource(name)
        .getLines
        .toVector
    var initial = (-1, -1)
    val obstacles =
      for
        (row, rowNumber) <- lines.zipWithIndex
        (cell, columnNumber) <- row.zipWithIndex
        if { if cell == '^' then initial = (rowNumber, columnNumber); cell == '#' }
      yield (rowNumber, columnNumber)
    require(initial != (-1, -1))
    Board(lines.size, initial, obstacles.toSet)

  /// part 1

  def part1(name: String): Int =
    val Outcome.Exit(size) = navigate(getInput(name)): @unchecked
    size

  test("part 1 sample"):
    assertEquals(part1("day06-sample.txt"), 41)
  test("part 1"):
    assertEquals(part1("day06.txt"), 4758)

  /// part 2

  def part2(name: String): Int =
    val board = getInput(name)
    val indices =
      for
        row <- 0 to board.size
        col <- 0 to board.size
      yield (row, col)
    indices.count: pos =>
      pos != board.initial &&
        !board.obstacles.contains(pos) &&
        navigate(board.copy(obstacles = board.obstacles + pos)) == Outcome.Loop

  test("part 2 sample"):
    assertEquals(part2("day06-sample.txt"), 6)
  test("part 2"):
    assertEquals(part2("day06.txt"), 1670)

end Day06
