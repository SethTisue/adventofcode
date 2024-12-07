class Day05 extends munit.FunSuite:

  type Rule = (Int, Int)
  case class Input(rules: List[Rule], updates: List[List[Int]])

  def middle[T](xs: Seq[T]): T =
    xs(xs.size / 2)

  /// reading & parsing

  def getInput(name: String): Input =
    val (section1, section2) =
      io.Source.fromResource(name)
        .getLines
        .span(_.nonEmpty)
    Input(
      rules = section1.map{case s"$n1|$n2" => (n1.toInt, n2.toInt)}.toList,
      updates = section2.drop(1).map(_.split(",").map(_.toInt).toList).toList)

  /// part 1

  def part1(name: String): Int =
    val input = getInput(name)
    input.updates.filter(up => isCorrect(input.rules, up)).map(middle).sum

  def isCorrect(rules: List[Rule], update: List[Int]): Boolean =
    rules.forall: rule =>
      val i1 = update.indexOf(rule._1)
      val i2 = update.indexOf(rule._2)
      i1 == -1 || i2 == -1 || i1 < i2

  test("part 1 sample"):
    assertEquals(part1("day05-sample.txt"), 143)
  test("part 1"):
    assertEquals(part1("day05.txt"), 4637)

  /// part 2

  def part2(name: String): Int =
    val input = getInput(name)
    input.updates.filterNot(up => isCorrect(input.rules, up))
      .map(in => fix(input.rules, in))
      .map(middle)
      .sum

  def fix(rules: List[Rule], up: List[Int]): List[Int] =
    val ordering = new Ordering[Int]:
      def compare(x: Int, y: Int): Int =
        if rules.contains((x, y)) then -1
        else if rules.contains((y, x)) then 0
        else if x < y then -1
        else if y < x then 1
        else 0
    up.sorted(ordering)

  test("part 2 sample"):
    assertEquals(part2("day05-sample.txt"), 123)
  test("part 2"):
    assertEquals(part2("day05.txt"), 6370)

end Day05
