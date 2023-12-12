/*
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
*/

class Day12 extends munit.FunSuite:

  /// core logic

  def matches(states: Vector[Char], counts: Vector[Int]): Int =
    if states.isEmpty
    then
      if counts.isEmpty
      then 1
      else 0
    else states.head match
      case '.' =>
        matches(states.tail, counts)
      case '#' =>
        val firstCount = counts.head
        if firstCount == 0
        then 0
        else matches(states.tail, (firstCount - 1) +: counts.tail)
      case '?' =>
        matches('#' +: states.tail, counts) +
          matches('.' +: states.tail, counts)
      case c =>
        throw new RuntimeException(s"invalid input char: $c")

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
      .tapEach(println)
      .map: (states, counts) =>
        matches(states, counts)
      .tapEach(println)
      .sum
  test("part 1 sample"):
    assertEquals(part1("day12-sample.txt"), 21)
/*
  test("part 1"):
    assertEquals(part1("day12.txt"), 0)

  /// part 2

  def part2(name: String): Int =
    getInput(name).size

  test("part 2 sample"):
    assertEquals(part2("day12-sample.txt"), 0)
  test("part 2"):
    assertEquals(part2("day12.txt"), 0)
*/

end Day12
