class Day12 extends munit.FunSuite:

  /// core logic

  def matches(states: Vector[Char], counts: Vector[Int], seen: Int = 0): Int =
    if states.isEmpty
    then
      if (seen == 0 && counts.isEmpty) || counts == Vector(seen)
      then 1
      else 0
    else states.head match
      case '.' =>
        if counts.headOption.contains(seen)
        then matches(states.tail, counts.tail)
        else if seen == 0
        then matches(states.tail, counts)
        else 0
      case '#' =>
        if counts.headOption.contains(seen)
        then 0
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

  def part1(name: String): Int =
    getInput(name)
      .map: (states, counts) =>
        matches(states, counts)
      .sum
  test("part 1 sample"):
    assertEquals(part1("day12-sample.txt"), 21)
  test("part 1"):
    assertEquals(part1("day12.txt"), 7361)

  /// part 2

  def part2(name: String): Int =
    getInput(name)
      .map: (states, counts) =>
        println((states, counts))
        matches(Vector.fill(5)('?' +: states).flatten.tail, Vector.fill(5)(counts).flatten)
      .sum

  test("part 2 sample"):
    assertEquals(part2("day12-sample.txt"), 525152)
  test("part 2"):
    assertEquals(part2("day12.txt"), 0)

end Day12
