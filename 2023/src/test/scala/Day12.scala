class Day12 extends munit.FunSuite:

  /// core logic

  lazy val matches: (Vector[Char], Vector[Int], Int) => Long =
    Memo.memoize:
      (states: Vector[Char], counts: Vector[Int], seen: Int) =>
        if states.isEmpty
        then
          if (seen == 0 && counts.isEmpty) || counts == Vector(seen)
          then 1L
          else 0L
        else states.head match
          case '.' =>
            if counts.headOption.contains(seen)
            then matches(states.tail, counts.tail, 0)
            else if seen == 0L
            then matches(states.tail, counts, 0)
            else 0L
          case '#' =>
            if counts.headOption.contains(seen)
            then 0L
            else matches(states.tail, counts, seen + 1)
          case '?' =>
            matches('#' +: states.tail, counts, seen) +
              matches('.' +: states.tail, counts, seen)
          case c =>
            require(false, s"invalid: $c"); ???

  /// reading & parsing

  def getInput(name: String): Vector[(Vector[Char], Vector[Int])] =
    def parse(line: String) =
      line.split(' ') match
        case Array(states, counts) =>
          (states.toVector,
            counts.split(',').map(_.toInt).toVector)
    io.Source.fromResource(name)
      .getLines
      .map(parse)
      .toVector

  /// part 1

  def part1(name: String): Long =
    getInput(name)
      .map: (states, counts) =>
        matches(states, counts, 0)
      .sum
  test("part 1 sample"):
    assertEquals(part1("day12-sample.txt"), 21L)
  test("part 1"):
    assertEquals(part1("day12.txt"), 7361L)

  /// part 2

  def part2(name: String): Long =
    getInput(name)
      .map: (states, counts) =>
        matches(Vector.fill(5)('?' +: states).flatten.tail, Vector.fill(5)(counts).flatten, 0)
      .sum

  test("part 2 sample"):
    assertEquals(part2("day12-sample.txt"), 525152L)
  test("part 2"):
    assertEquals(part2("day12.txt"), 83317216247365L)

end Day12
