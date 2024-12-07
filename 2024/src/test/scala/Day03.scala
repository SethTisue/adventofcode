class Day03 extends munit.FunSuite:

  def getInput(name: String): String =
    io.Source.fromResource(name).mkString

  object Int:
    def unapply(s: String) =
      s.toIntOption

  // part 1

  test("part 1 sample"):
    assertEquals(
      part1(getInput("day03-sample1.txt")),
      161)

  test("part 1"):
    assertEquals(
      part1(getInput("day03.txt")),
      174960292)

  def part1(input: String): Int =
    input.tails.flatMap:
      case s"mul(${Int(x)},${Int(y)})$_" =>
        Some(x * y)
      case _ => None
    .sum

  // part 2

  test("part 2 sample"):
    assertEquals(
      part2(getInput("day03-sample2.txt")),
      48)

  test("part 2"):
    assertEquals(
      part2(getInput("day03.txt")),
      56275602)

  @annotation.tailrec
  final def part2(input: String, enabled: Boolean = true, result: Int = 0): Int =
    input match
      case "" => result
      case s"don't()$rest" =>
        part2(rest, enabled = false, result)
      case s"do()$rest" =>
        part2(rest, enabled = true, result)
      case s"mul(${Int(x)},${Int(y)})$rest" if enabled =>
        part2(rest, enabled, result + x * y)
      case _ =>
        part2(input.tail, enabled, result)

end Day03
