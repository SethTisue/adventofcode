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
      require(Packet.toString(result) == s)
      result
    def parse(s: String): (Packet, String) =
      val (digits, more) = s.span(_.isDigit)
      if digits.nonEmpty then
        (Packet.Number(digits.toInt), more)
      else
        require(s.head == '[', s.head.toString)
        def recurse(s2: String): (List[Packet], String) =
          if s2.head == ']'
          then (Nil, s2.tail)
          else
            val (next, s3) = parse(s2)
            val s3b = if s3.head == ',' then s3.tail else s3
            val (rest, s4) = recurse(s3b)
            (next :: rest, s4)
        val (xs, foo) = recurse(s.tail)
        (Packet.Nested(xs), foo)

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
