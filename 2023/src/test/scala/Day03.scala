class Day03 extends munit.FunSuite:

  /// reading and parsing

  def getInput(name: String): Vector[String] =
    val rows =
      io.Source.fromResource(name)
        .getLines.toVector
    val emptyRow = ".".repeat(rows.head.size)
    emptyRow +: rows :+ emptyRow

  /// part 1

  def part1(name: String): Int =
    val schematic = getInput(name)
    schematic.sliding(3)
      .collect:
        case Vector(prev, cur, next) =>
          partNumbers(prev, cur, next).sum
      .sum

  def isSymbol(c: Char): Boolean =
    c != '.' && !c.isDigit

  def partNumbers(prev: String, cur: String, next: String, justSawSymbol: Boolean = false): Vector[Int] =
    if cur.isEmpty
    then Vector()
    else
      val symbolHere = isSymbol(prev.head) || isSymbol(cur.head) || isSymbol(next.head)
      if !cur.head.isDigit
      then partNumbers(prev.tail, cur.tail, next.tail, symbolHere)
      else
        val (digits, rest) = cur.span(_.isDigit)
        if digits.isEmpty
        then partNumbers(prev.tail, cur.tail, next.tail, symbolHere)
        else
          def recurse =
            partNumbers(
              prev.drop(digits.size),
              cur.drop(digits.size),
              next.drop(digits.size),
              symbolHere)
          if justSawSymbol ||
            prev.take(digits.size + 1).exists(isSymbol) ||
            cur.take(digits.size + 1).exists(isSymbol) ||
            next.take(digits.size + 1).exists(isSymbol)
          then digits.mkString.toInt +: recurse
          else recurse

  test("part 1 sample"):
    assertEquals(4361, part1("day03-sample.txt"))
  test("part 1"):
    assertEquals(551094, part1("day03.txt"))

end Day03
