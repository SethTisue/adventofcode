class Day3 extends munit.FunSuite:

  val sample =
    List("00100", "11110", "10110", "10111", "10101", "01111",
     "00111", "11100", "10000", "11001", "00010", "01010")

  def fromBinary(bits: String): Int =
    Integer.parseInt(bits, 2)

  // Part 1

  def mostCommonBit(chars: Seq[Char]): Char =
    if (chars.count(_ == '1') * 2 >= chars.size)
    then '1' else '0'

  def flip(bit: Char): Char =
    if (bit == '0')
    then '1' else '0'

  def powerConsumption(xs: List[String]): Int =
    val gammaBits =
      xs.head.indices.map(i => mostCommonBit(xs.map(_(i)))).mkString
    val epsilonBits =
      gammaBits.map(flip)
    fromBinary(gammaBits) * fromBinary(epsilonBits)

  test("part 1 sample") {
    assertEquals(powerConsumption(sample), 198)
  }

  test("part 1 real") {
    val input = io.Source.fromFile("day3.txt").getLines.toList
    assertEquals(powerConsumption(input), 4139586)
  }

  // Part 2

  def oxygenRating(xs: List[String], index: Int = 0): Int =
    val bit = mostCommonBit(xs.map(_(index)))
    val keep = xs.filter(_(index) == bit)
    if (keep.size == 1)
    then fromBinary(keep.head)
    else oxygenRating(keep, index + 1)

  def scrubberRating(xs: List[String], index: Int = 0): Int =
    val bit = flip(mostCommonBit(xs.map(_(index))))
    val keep = xs.filter(_(index) == bit)
    if (keep.size == 1)
    then fromBinary(keep.head)
    else scrubberRating(keep, index + 1)

  test("part 2 sample") {
    assertEquals(oxygenRating(sample) * scrubberRating(sample), 230)
  }

  test("part 2 real") {
    val input = io.Source.fromFile("day3.txt").getLines.toList
    assertEquals(oxygenRating(input) * scrubberRating(input), 1800151)
  }

end Day3
