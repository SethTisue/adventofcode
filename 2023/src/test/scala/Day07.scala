class Day07 extends munit.FunSuite:

  /// data model

  type Hand = Seq[Char]
  type Bid = Int

  def cardCounts(hand: Hand): Seq[Int] =
    hand.groupBy(identity).map(_._2.size).toSeq.sorted.reverse

  val cards = "23456789TJQKA"

  val types = Seq(
    Seq(1, 1, 1, 1, 1), // high card
    Seq(2, 1, 1, 1),    // one pair
    Seq(2, 2, 1),       // two pair
    Seq(3, 1, 1),       // three of a kind
    Seq(3, 2),          // full house
    Seq(4, 1),          // four of a kind
    Seq(5),             // five of a kind
  )

  given Ordering[Hand] with
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
            types.indexOf(cardCounts(h1)).ensuring(_ != -1, s"$h1 ${cardCounts(h1)}"),
            types.indexOf(cardCounts(h2)).ensuring(_ != -1))
      if result != 0
      then result
      else tieBreaker(h1, h2)

  /// part 1

  def getInput(name: String): Vector[(Hand, Bid)] =
    io.Source.fromResource(name)
      .getLines
      .map:
        case s"$hand $bid" =>
          (hand.toSeq, bid.toInt)
      .toVector

  def part1(name: String): Long =
    import util.chaining.*
    getInput(name)
      .sortBy(_._1)
      .tapEach(println)
      .map(_._2)
      .zipWithIndex
      .map: (bid, index) =>
        bid * (index + 1)
      .sum

  test("part 1 sample"):
    assertEquals(part1("day07-sample.txt"), 6440L)
  test("part 1"):
    assertEquals(part1("day07.txt"), 0L)

end Day07
