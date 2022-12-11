class Day10 extends munit.FunSuite:

  // part 1

  def getInput(file: String): Iterator[Int] =
    io.Source.fromResource(file).getLines
      .flatMap {
        case "noop"          => List(0)
        case s"addx $amount" => List(0, amount.toInt)
      }

  def signalStrengthSum(deltas: Iterator[Int]): Int =
    val xs = deltas.scanLeft(1)(_ + _).to(IndexedSeq)
    (20 to xs.size by 40)
      .map(n => xs(n - 1) * n)
      .sum

  def testDay10Part1(name: String, file: String, expected: Int) =
    test(s"day 10 $name") {
      assertEquals(signalStrengthSum(getInput(file)), expected)
    }

  testDay10Part1("part 1 sample", "day10-sample.txt", 13140)
  testDay10Part1("part 1",        "day10.txt",        14320)

end Day10
