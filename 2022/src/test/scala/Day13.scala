class Day13 extends munit.FunSuite:

  // shared code

  enum Packet:
    case Number(n: Int)
    case Nested(ps: List[Packet])
  object Packet:
    def toString(p: Packet): String = p match
      case Number(n) => n.toString
      case Nested(ps) => ps.map(Packet.toString).mkString("[",",","]")
    def fromString(s: String): Packet =
      val (result, remaining) = parse(s)
      require(remaining.isEmpty, remaining)
      require(Packet.toString(result) == s, Packet.toString(result))
      result
    // like monadic parser combinators, but handcoded
    def parse(s: String): (Packet, String) =
      val (digits, s2) = s.span(_.isDigit)
      if digits.nonEmpty then
        (Number(digits.toInt), s2)
      else
        require(s.head == '[')
        def recurse(s2: String): (List[Packet], String) =
          s2.head match
            case ',' => recurse(s2.tail)
            case ']' => (Nil, s2.tail)
            case _ =>
              val (next, s3) = parse(s2)
              val (rest, s4) = recurse(s3)
              (next :: rest, s4)
        val (ps, s2) = recurse(s.tail)
        (Nested(ps), s2)

  import Packet.*

  given Ordering[Packet] with
    def compare(p1: Packet, p2: Packet): Int =
      (p1, p2) match
        case (Number(n1), Number(n2)) =>
          n1.compare(n2)
        case (Number(_), _) =>
          compare(Nested(List(p1)), p2)
        case (_, Number(_)) =>
          compare(p1, Nested(List(p2)))
        case (Nested(Nil), Nested(Nil)) =>
          0
        case (Nested(_), Nested(Nil)) =>
          1
        case (Nested(Nil), Nested(_)) =>
          -1
        case (Nested(l1), Nested(l2)) =>
          compare(l1.head, l2.head) match
            case 1 => 1
            case -1 => -1
            case 0 => compare(Nested(l1.tail), Nested(l2.tail))

  def getInput(file: String): Iterator[Packet] =
    io.Source.fromResource(file)
      .getLines
      .filter(_.nonEmpty)
      .map(Packet.fromString)

  // part 1

  def solvePart1(input: Iterator[(Packet, Packet)]): Int =
    import Ordering.Implicits.*
    input.zipWithIndex
      .collect{case ((p1, p2), i) if p1 < p2 => i + 1}
      .sum

  def testDay13Part1(name: String, file: String, expected: Int) =
    test(s"day 13 $name") {
      val input = getInput(file).grouped(2).map{case Seq(p1, p2) => (p1, p2)}
      assertEquals(solvePart1(input), expected)
    }

  testDay13Part1("part 1 sample", "day13-sample.txt",   13)
  testDay13Part1("part 1",        "day13.txt",        5605)

  // part 2

  def solvePart2(input: Iterator[Packet]): Int =
    def divider(n: Int) = Nested(List(Nested(List(Number(n)))))
    val sorted = (input.toSeq :+ divider(2) :+ divider(6)).sorted
    (sorted.indexOf(divider(2)) + 1) *
      (sorted.indexOf(divider(6)) + 1)

  def testDay13Part2(name: String, file: String, expected: Int) =
    test(s"day 13 $name") {
      val input = getInput(file)
      assertEquals(solvePart2(input), expected)
    }

  testDay13Part2("part 2 sample", "day13-sample.txt",   140)
  testDay13Part2("part 2",        "day13.txt",        24969)

end Day13
