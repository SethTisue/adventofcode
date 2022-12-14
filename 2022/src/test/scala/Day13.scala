class Day13 extends munit.FunSuite:

  def getInput(file: String): Iterator[String] =
    io.Source.fromResource(file)
      .getLines

  def solve(input: Iterator[String]): Int =
    0

  def testDay13(name: String, file: String, expected: Int) =
    test(s"day 13 $name") {
      val input = getInput(file)
      assertEquals(solve(input), expected)
    }

  testDay13("part 1 sample", "day13-sample.txt", 13)

end Day13
