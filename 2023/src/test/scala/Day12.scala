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
  test("part 1 sample 1"):
    assertEquals(part1("day12-sample1.txt"), 6)
  test("part 1 sample 2"):
    assertEquals(part1("day12-sample2.txt"), 21)
  test("part 1"):
    assertEquals(part1("day12.txt"), 7361)

end Day12
