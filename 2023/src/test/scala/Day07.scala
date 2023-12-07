// This code is okay. One change I'd make, after seeing others'
// solutions, is to have `improve`, the method that upgrades a hand by
// replacing jokers, operate on card counts rather than on the cards
// themselves. I didn't think of that.

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

  def compareByType(h1: Hand, h2: Hand): Int =
    summon[Ordering[Int]]
      .compare(
        types.indexOf(cardCounts(h1)),
        types.indexOf(cardCounts(h2)))

  def compareCardWise(cards: String, h1: Hand, h2: Hand): Int =
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
      else compareCardWise(cards, h1.tail, h2.tail)

  def ordering(cards: String): Ordering[Hand] =
    (h1: Hand, h2: Hand) =>
      val result = compareByType(h1, h2)
      if result != 0
      then result
      else compareCardWise(cards, h1, h2)

  def winnings(input: Vector[(Hand, Bid)])(using Ordering[Hand]): Int =
    input
      .sortBy(_._1)
      .map(_._2)
      .zipWithIndex
      .map: (bid, index) =>
        bid * (index + 1)
      .sum

  def improve(h: Hand): Hand =
    if h.mkString == "JJJJJ"
    then "AAAAA".toSeq
    else
      val commonest =
        h.filterNot(_ == 'J')
          .groupBy(identity)
          .maxBy(_._2.size)
          ._1
      h.map: card =>
        if card == 'J'
        then commonest
        else card

  /// reading & parsing

  def getInput(name: String): Vector[(Hand, Bid)] =
    io.Source.fromResource(name)
      .getLines
      .map:
        case s"$hand $bid" =>
          (hand.toSeq, bid.toInt)
      .toVector

  /// part 1

  def part1(name: String): Int =
    given Ordering[Hand] = ordering("23456789TJQKA")
    winnings(getInput(name))

  test("part 1 sample"):
    assertEquals(part1("day07-sample.txt"), 6440)
  test("part 1"):
    assertEquals(part1("day07.txt"), 248569531)

  /// part 2

  object part2Ordering extends Ordering[Hand]:
    def compare(h1: Hand, h2: Hand): Int =
      val improved1 = improve(h1)
      val improved2 = improve(h2)
      val result = compareByType(improved1, improved2)
      if result != 0
      then result
      else
        compareCardWise("J23456789TQKA", h1, h2)

  def part2(name: String): Int =
    given Ordering[Hand] = part2Ordering
    winnings(getInput(name))

  test("part 2 sample"):
    assertEquals(part2("day07-sample.txt"), 5905)
  test("part 2"):
    assertEquals(part2("day07.txt"), 250382098)

end Day07
