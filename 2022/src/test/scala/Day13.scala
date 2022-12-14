class Day13 extends munit.FunSuite:

  enum Packet:
    case Number(n: Int)
    case Nested(ps: List[Packet])
  object Packet:
    def toString(p: Packet): String = p match
      case Packet.Number(n) => n.toString
      case Packet.Nested(ps) => ps.map(Packet.toString).mkString("[",",","]")
    def fromString(s: String): Packet =
      val (result, remaining) = parse(s)
      require(remaining.isEmpty, remaining)
      require(Packet.toString(result) == s, Packet.toString(result))
      result
    // like monadic parser combinators, but handcoded
    def parse(s: String): (Packet, String) =
      val (digits, s2) = s.span(_.isDigit)
      if digits.nonEmpty then
        (Packet.Number(digits.toInt), s2)
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
        (Packet.Nested(ps), s2)

  def compare(p1: Packet, p2: Packet): Int =
    (p1, p2) match
      case (Packet.Number(n1), Packet.Number(n2)) =>
        n1.compare(n2)
      case (Packet.Number(_), _) =>
        compare(Packet.Nested(List(p1)), p2)
      case (_, Packet.Number(_)) =>
        compare(p1, Packet.Nested(List(p2)))
      case (Packet.Nested(Nil), Packet.Nested(Nil)) =>
        0
      case (Packet.Nested(_), Packet.Nested(Nil)) =>
        1
      case (Packet.Nested(Nil), Packet.Nested(_)) =>
        -1
      case (Packet.Nested(l1), Packet.Nested(l2)) =>
        compare(l1.head, l2.head) match
          case 1 => 1
          case -1 => -1
          case 0 => compare(Packet.Nested(l1.tail), Packet.Nested(l2.tail))

  def getInput(file: String): Iterator[(Packet, Packet)] =
    io.Source.fromResource(file)
      .getLines
      .filter(_.nonEmpty)
      .map(Packet.fromString)
      .grouped(2)
      .map{case Seq(p1, p2) => (p1, p2)}

  def solve(input: Iterator[(Packet, Packet)]): Int =
    input.zipWithIndex
      .collect{
        case ((p1, p2), i) if compare(p1, p2) == -1 =>
          i + 1}
      .sum

  def testDay13(name: String, file: String, expected: Int) =
    test(s"day 13 $name") {
      val input = getInput(file)
      assertEquals(solve(input), expected)
    }

  testDay13("part 1 sample", "day13-sample.txt",   13)
  testDay13("part 1",        "day13.txt",        5605)

end Day13
