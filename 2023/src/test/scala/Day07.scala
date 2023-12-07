class Day07 extends munit.FunSuite:

  /// data model

  type Hand = Seq[Char]
  type Bid = Int

  def cardCounts(hand: Hand): Seq[Int] =
    hand.groupBy(identity).map(_._2.size).toSeq.sorted.reverse

  val types = Seq(
    Seq(1, 1, 1, 1, 1), // high card
    Seq(2, 1, 1, 1),    // one pair
    Seq(2, 2, 1),       // two pair
    Seq(3, 1, 1),       // three of a kind
    Seq(3, 2),          // full house
    Seq(4, 1),          // four of a kind
    Seq(5),             // five of a kind
  )

  def winnings(input: Vector[(Hand, Bid)])(using Ordering[Hand]): Long =
    input
      .sortBy(_._1)
      .map(_._2)
      .zipWithIndex
      .map: (bid, index) =>
        bid * (index + 1)
      .sum

  /// reading & parsing

  def getInput(name: String): Vector[(Hand, Bid)] =
    io.Source.fromResource(name)
      .getLines
      .map:
        case s"$hand $bid" =>
          (hand.toSeq, bid.toInt)
      .toVector

  /// part 1

  def part1Ordering(cards: String) =
    new Ordering[Hand]:
      def tieBreaker(h1: Hand, h2: Hand): Int =
        if h1.isEmpty
        then
          require(h2.isEmpty)
          0
        else
          val result =
            summon[Ordering[Int]].compare(
              cards.indexOf(h1.head),
              cards.indexOf(h2.head))
          if result != 0
          then result
          else tieBreaker(h1.tail, h2.tail)
      def compare(h1: Hand, h2: Hand): Int =
        val result =
          summon[Ordering[Int]]
            .compare(
              types.indexOf(cardCounts(h1)),
              types.indexOf(cardCounts(h2)))
        if result != 0
        then result
        else tieBreaker(h1, h2)

  def part1(name: String): Long =
    given Ordering[Hand] = part1Ordering("23456789TJQKA")
    winnings(getInput(name))

  test("part 1 sample"):
    assertEquals(part1("day07-sample.txt"), 6440L)
  test("part 1"):
    assertEquals(part1("day07.txt"), 248569531L)

  /// part 2

  object part2Ordering extends Ordering[Hand]:
    val delegate = part1Ordering("J23456789TQKA")
    def compare(h1: Hand, h2: Hand): Int =
      delegate.compare(h1, h2)

  def part2(name: String): Long =
    given Ordering[Hand] = part2Ordering
    winnings(getInput(name))

  test("part 2 sample"):
    assertEquals(part2("day07-sample.txt"), 5905L)
/*
  test("part 2"):
    assertEquals(part2("day07.txt"), 0)
 */

end Day07
