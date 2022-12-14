class Day13 extends munit.FunSuite:

  enum Packet:
    case Number(n: Int)
    case Nested(ps: List[Packet])
  object Packet:
    def fromString(s: String): Packet =
      val (result, remaining) = parse(s)
      require(remaining.isEmpty, remaining)
      println(result)
      result
    def parse(s: String): (Packet, String) =
      val (digits, more) = s.span(_.isDigit)
      if digits.nonEmpty then
        (Packet.Number(digits.toInt), more)
      else
        require(s.head == '[')
        var remaining = s.tail
        var buf = Vector[Packet]()
        while remaining.head != ']' do
          val (next, rem2) = parse(remaining)
          buf :+= next
          if rem2.head == ',' then
            remaining = rem2.tail
          else
            require(rem2.head == ']')
            remaining = rem2
        end while
        (Packet.Nested(buf.toList), remaining.tail)

  def getInput(file: String): Iterator[String] =
    io.Source.fromResource(file)
      .getLines

  def solve(input: Iterator[String]): Int =
    def one = Packet.fromString(input.next()).asInstanceOf[Packet.Nested].ps.size
    List(one, one, { input.next(); one }, one).sum

  def testDay13(name: String, file: String, expected: Int) =
    test(s"day 13 $name") {
      val input = getInput(file)
      assertEquals(solve(input), expected)
    }

  testDay13("part 1 sample", "day13-sample.txt", 13)

end Day13
