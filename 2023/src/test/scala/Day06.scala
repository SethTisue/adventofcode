class Day06 extends munit.FunSuite:

  /// data model

  case class Race(time: Long, record: Long):
    def ways: Long =
      (1L until time)
       .map: holdTime =>
         holdTime * (time - holdTime)
       .count:
         _ > record

  /// part 1

  def getInput(name: String): (Seq[Int], Seq[Int]) =
    val lines = io.Source.fromResource(name).getLines
    def parse(line: String): Seq[Int] =
      val s"$_: $rest" = line: @unchecked
      rest.trim.split("\\s+").map(_.toInt).toVector
    (parse(lines.next()), parse(lines.next()))

  def part1(name: String): Long =
    val (times, records) = getInput(name)
    times.zip(records).map(Race(_, _))
      .map(_.ways)
      .product

  test("part 1 sample"):
    assertEquals(part1("day06-sample.txt"), 288L)
  test("part 1"):
    assertEquals(part1("day06.txt"), 781200L)

  /// part 2

  def part2(name: String): Long =
    val (times, records) = getInput(name)
    Race(times.mkString.toLong, records.mkString.toLong)
      .ways

  test("part 2 sample"):
    assertEquals(part2("day06-sample.txt"), 71503L)
  test("part 2"):
    assertEquals(part2("day06.txt"), 49240091L)

end Day06
