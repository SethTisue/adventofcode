class Day06 extends munit.FunSuite:

  /// data model

  case class Race(time: Int, record: Int)

  /// reading and parsing

  def getInput(name: String): Vector[Race] =
    val lines = io.Source.fromResource(name).getLines
    val times =
      val s"Time: $rest" = lines.next(): @unchecked
      rest.trim.split("\\s+").map(_.toInt).toVector
    val records =
      val s"Distance: $rest" = lines.next(): @unchecked
      rest.trim.split("\\s+").map(_.toInt).toVector
    for (time, record) <- times.zip(records)
    yield Race(time, record)

  /// part 1

  def part1(name: String): Long =
    getInput(name)
      .map: race =>
        (1 until race.time)
          .map(holdTime => holdTime * (race.time - holdTime))
          .count(_ > race.record)
      .product

  test("part 1 sample"):
    assertEquals(part1("day06-sample.txt"), 288L)
  test("part 1"):
    assertEquals(part1("day06.txt"), 781200L)

/*
  /// part 2

  def part2(name: String): Long =
    val input = getInput(name)
    0L

  test("part 2 sample"):
    assertEquals(part2("day06-sample.txt"), 0L)
  test("part 2"):
    assertEquals(part2("day06.txt"), 0L)
 */

end Day06
