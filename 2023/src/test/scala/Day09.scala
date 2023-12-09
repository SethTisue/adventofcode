class Day09 extends munit.FunSuite:

  /// core logic

  def extrapolate(xs: Seq[Int]): Int =
    if xs.forall(_ == xs.head)
    then xs.head
    else
      xs.last + extrapolate(
        xs.tail.lazyZip(xs)
          .map(_ - _)
          .toSeq)

  /// reading & parsing

  def getInput(name: String): Seq[Seq[Int]] =
    io.Source.fromResource(name)
      .getLines
      .map(_.split(' ').map(_.toInt).toSeq)
      .toSeq

  /// part 1

  def part1(name: String): Int =
    getInput(name)
      .map(extrapolate)
      .sum

  test("part 1 sample"):
    assertEquals(part1("day09-sample.txt"), 114)
  test("part 1"):
    assertEquals(part1("day09.txt"), 1637452029)

  /// part 2

  def part2(name: String): Int =
    getInput(name)
      .map(_.reverse)
      .map(extrapolate)
      .sum

  test("part 2 sample"):
    assertEquals(part2("day09-sample.txt"), 2)
  test("part 2"):
    assertEquals(part2("day09.txt"), 908)

end Day09
