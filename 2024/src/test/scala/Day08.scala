class Day08 extends munit.FunSuite:

  /// core logic

  case class Antenna(row: Int, column: Int, frequency: Char)
  case class Board(size: Int, antennae: Set[Antenna])

  /// reading & parsing

  def getInput(name: String): Board =
    val lines =
      io.Source.fromResource(name)
        .getLines
        .toVector
    Board(lines.size,
      for row <- (0 until lines.size).toSet
          column <- 0 until lines.size
          freq = lines(row).charAt(column)
          if freq != '.'
      yield Antenna(row, column, freq))

  /// part 1

  def part1(name: String): Int =
    val board = getInput(name)
    val antinodes =
      for a1 <- board.antennae
          a2 <- board.antennae
          if a1.frequency == a2.frequency && a1 != a2
          anti1 = (a1.row * 2 - a2.row, a1.column * 2 - a2.column)
          anti2 = (a2.row * 2 - a1.row, a2.column * 2 - a1.column)
      yield Set(anti1, anti2)
    def inBounds(pos: (Int, Int)) =
      pos._1 >= 0 && pos._1 < board.size && pos._2 >=0 && pos._2 < board.size
    antinodes.flatten.filter(inBounds).size

  test("part 1 sample"):
    assertEquals(part1("day08-sample.txt"), 14)
  test("part 1"):
    assertEquals(part1("day08.txt"), 222)

  /// part 2

  def part2(name: String): Int =
    val board = getInput(name)
    def inBounds(pos: (Int, Int)) =
      pos._1 >= 0 && pos._1 < board.size && pos._2 >= 0 && pos._2 < board.size
    val antinodes =
      for a1 <- board.antennae
          a2 <- board.antennae
          if a1.frequency == a2.frequency && a1 != a2
          antis1 = LazyList.iterate((a2.row, a2.column)){case (r, c) =>
              (r + a2.row - a1.row, c + a2.column - a1.column)}
            .takeWhile(inBounds).toSet
          antis2 = LazyList.iterate((a1.row, a1.column)){case (r, c) =>
              (r + a1.row - a2.row, c + a1.column - a2.column)}
            .takeWhile(inBounds).toSet
      yield (antis1 ++ antis2)
    antinodes.flatten.filter(inBounds).size

  test("part 2 sample"):
    assertEquals(part2("day08-sample.txt"), 34)
  test("part 2"):
    assertEquals(part2("day08.txt"), 884)

end Day08
