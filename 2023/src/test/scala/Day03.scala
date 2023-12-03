class Day03 extends munit.FunSuite:

  // data model

  enum Entry:
    case Empty
    case Symbol
    case Digit(n: Int)

  object Entry:
    def fromChar(c: Char): Entry =
      if c == '.' then Empty
      else if c.isDigit then Digit(c - '0')
      else Symbol

  type Schematic = Vector[Vector[Entry]]

  // reading and parsing

  def getInput(name: String): Schematic =
    val rows =
      io.Source.fromResource(name)
        .getLines.toVector.map: line =>
          line.toVector.map(Entry.fromChar)
    val emptyRow = Vector.fill(rows.head.size)(Entry.Empty)
    emptyRow +: rows :+ emptyRow

  // part 1

  def part1(name: String): Int =
    val schematic = getInput(name)
    val parts =
      for case Vector (prev, cur, next) <- schematic
      if current

  test("part 1 sample"):
    assertEquals(4361, part1("day03-sample.txt"))
/*
  test("part 1"):
    assertEquals(0, part1("day03.txt"))
*/

/*
  // part 2

  def part2(name: String): Int = ???

  test("part 2 sample"):
    assertEquals(0, part2("day02-sample.txt"))
  test("part 2"):
    assertEquals(0, part2("day02.txt"))
*/

end Day03
