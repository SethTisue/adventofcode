class Day03 extends munit.FunSuite:

  /// reading and parsing

  // to aid uniform processing, we add empty rows to the top and
  // bottom, and we add `...` to the start and end of each row

  def getInput(name: String): Vector[String] =
    val rows =
      io.Source.fromResource(name)
        .getLines.toVector.map(line => s"...$line...")
    val emptyRow = ".".repeat(rows.head.size)
    emptyRow +: rows :+ emptyRow

  /// part 1

  // we loop over the input rows with `.sliding(3)` so we can see
  // the row above and the row below us

  def part1(name: String): Int =
    val schematic = getInput(name)
    schematic.sliding(3)
      .collect:
        case Vector(prev, cur, next) =>
          partNumbers(prev, cur, next).sum
      .sum

  def isSymbol(c: Char): Boolean =
    c != '.' && !c.isDigit

  // and then we recurse over the tails of the rows. when we see a
  // number at the start of the current tail, we check if it is
  // touching a symbol. (and so we need an extra boolean to keep
  // remember if we just recursed past a symbol)

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

  /// part 2

  // this part uses the same structure: `sliding(3)` over rows,
  // recurse over row tails

  def part2(name: String): Int =
    val schematic = getInput(name)
    schematic.sliding(3)
      .collect:
        case Vector(prev, cur, next) =>
          gearRatios(prev, cur, next).sum
      .sum

  // if there is a digit at the given position in the string,
  // return the entire integer that it's a part of

  def numberAt(row: String, pos: Int): Option[Int] =
    if !row(pos).isDigit
    then None
    else
      val (part1, part2) = row.splitAt(pos)
      val digits1 = part1.reverse.takeWhile(_.isDigit).reverse
      val digits2 = part2.takeWhile(_.isDigit)
      Some((digits1 ++ digits2).mkString.toInt)

  // the part numbers are never longer than 3 digits, so we
  // only need to consider rectangles of the form:
  //   .......
  //   ...*...
  //   .......
  // when we see the * in the center, we look for adjacent numbers

  def gearRatios(prev: String, cur: String, next: String): Vector[Int] =
    if cur.size < 7
    then Vector()
    else
      def recurse = gearRatios(prev.tail, cur.tail, next.tail)
      if cur(3) != '*'
      then recurse
      else
        val left = numberAt(cur, 2)
        val right = numberAt(cur, 4)
        val top = numberAt(prev, 3)
        val bottom = numberAt(next, 3)
        val topLeft = if top.isDefined then None else numberAt(prev, 2)
        val topRight = if top.isDefined then None else numberAt(prev, 4)
        val bottomLeft = if bottom.isDefined then None else numberAt(next, 2)
        val bottomRight = if bottom.isDefined then None else numberAt(next, 4)
        val parts =
          List(left, right, top, bottom, topLeft, topRight, bottomLeft, bottomRight)
            .flatten
        if parts.size == 2
        then parts.product +: recurse
        else recurse

  test("part 2 sample"):
    assertEquals(467835, part2("day03-sample.txt"))
  test("part 2"):
    assertEquals(80179647, part2("day03.txt"))

end Day03
