class Day04 extends munit.FunSuite:

  /// data model

  type Card = Int

  /// reading and parsing

  def getInput(name: String): Vector[Card] =
    def parse(s: String): Set[Int] =
      s.split(' ').filter(_.nonEmpty).map(_.toInt).toSet
    io.Source.fromResource(name)
      .getLines.toVector
      .map:
        case s"Card $_: $part1 | $part2" =>
          val (nums1, nums2) = (parse(part1), parse(part2))
          nums1.intersect(nums2).size

  /// part 1

  def part1(name: String): Int =
    def score(card: Card): Int =
      math.pow(2, card).toInt / 2
    getInput(name).map(score).sum

  test("part 1 sample"):
    assertEquals(part1("day04-sample.txt"), 13)
  test("part 1"):
    assertEquals(part1("day04.txt"), 24175)

  /// part 2

  def part2(name: String): Int =
    val input = getInput(name)
    def recurse(cards: Vector[Card], points: Vector[Int]): Vector[Int] =
      if cards.tail.isEmpty
      then points
      else
        val (win, lose) = points.tail.splitAt(cards.head)
        points.head +: recurse(cards.tail, win.map(_ + points.head) ++ lose)
    recurse(input, Vector.fill(input.size)(1))
      .sum

  test("part 2 sample"):
    assertEquals(part2("day04-sample.txt"), 30)
  test("part 2"):
    assertEquals(part2("day04.txt"), 18846301)

end Day04
