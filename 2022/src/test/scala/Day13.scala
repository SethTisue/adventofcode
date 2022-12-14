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
    // this is basically parser combinators but handcoded
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

  def getInput(file: String): Iterator[Packet] =
    io.Source.fromResource(file)
      .getLines
      .filter(_.nonEmpty)
      .map(Packet.fromString)

  def solve(input: Iterator[Packet]): Int =
    input.size

  def testDay13(name: String, file: String, expected: Int) =
    test(s"day 13 $name") {
      val input = getInput(file)
      assertEquals(solve(input), expected)
    }

  testDay13("part 1 sample", "day13-sample.txt", 13)

end Day13
