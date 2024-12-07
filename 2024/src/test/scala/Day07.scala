class Day07 extends munit.FunSuite:

  /// reading & parsing

  case class Equation(result: Long, numbers: Seq[Long])

  def getInput(name: String): Seq[Equation] =
    io.Source.fromResource(name)
      .getLines
      .map: line =>
        line match
          case s"$result: $numbers" =>
            Equation(result.toLong, numbers.split(' ').map(_.toLong).toSeq)
      .toSeq

  /// part 1

  def part1(name: String): Long =
    getInput(name)
      .map(eq => eq.copy(numbers = eq.numbers.reverse))
      .filter(isPossible1)
      .map(_.result)
      .sum

  def isPossible1(eq: Equation): Boolean =
    eq.numbers match
      case Seq(n) =>
        n == eq.result
      case n +: more =>
        isPossible1(Equation(eq.result - n, eq.numbers.tail)) ||
          (eq.result % n == 0 && isPossible1(Equation(eq.result / n, eq.numbers.tail)))

  test("part 1 sample"):
    assertEquals(part1("day07-sample.txt"), 3749L)
  test("part 1"):
    assertEquals(part1("day07.txt"), 6231007345478L)

  /// part 2

  def part2(name: String): Long =
    getInput(name)
      .map(eq => eq.copy(numbers = eq.numbers.reverse))
      .filter(isPossible2)
      .map(_.result)
      .sum

  def isPossible2(eq: Equation): Boolean =
    eq.numbers match
      case Seq(n) =>
        n == eq.result
      case n +: more =>
        isPossible2(Equation(eq.result - n, eq.numbers.tail)) ||
          (eq.result % n == 0 && isPossible2(Equation(eq.result / n, eq.numbers.tail))) ||
        (eq.result >= 0 && eq.result.toString.endsWith(n.toString) && eq.result.toString.size > n.toString.size && isPossible2(Equation(eq.result.toString.dropRight(n.toString.size).toLong, eq.numbers.tail)))

  test("part 2 sample"):
    assertEquals(part2("day07-sample.txt"), 11387L)
  test("part 2"):
    assertEquals(part2("day07.txt"), 333027885676693L)

end Day07
