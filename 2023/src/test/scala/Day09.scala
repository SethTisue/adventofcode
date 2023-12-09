class Day09 extends munit.FunSuite:

  /// core logic

  def extrapolate(xs: Seq[Int]): Int =
    if xs.forall(_ == xs.head)
    then xs.head
    else
      xs.last + extrapolate(
        xs.lazyZip(xs.tail)
          .map((x1, x2) => x2 - x1)
          .toSeq)

  /// reading & parsing

  def getInput(name: String): Vector[Vector[Int]] =
    io.Source.fromResource(name)
      .getLines
      .map(_.split(' ').map(_.toInt).toVector)
      .toVector

  /// part 1

  def part1(name: String): Int =
    getInput(name).map(extrapolate).sum

  test("part 1 sample"):
    assertEquals(part1("day09-sample.txt"), 114)
  test("part 1"):
    assertEquals(part1("day09.txt"), 1637452029)

end Day09
