class Day10 extends munit.FunSuite:

  def signalStrengthSum(deltas: Iterator[Int]): Int =
    val xs = deltas.scanLeft(1)(_ + _).to(IndexedSeq)
    (20 to xs.size by 40)
      .map(n => xs(n - 1) * n)
      .sum

  def testDay10(name: String, file: String, expected: Int) =
    test(s"day 10 $name") {
      val input: Iterator[Int] =
        io.Source.fromResource(file).getLines
          .flatMap {
            case "noop"          => List(0)
            case s"addx $amount" => List(0, amount.toInt)
          }
      assertEquals(signalStrengthSum(input), expected)
    }

  testDay10("part 1 sample", "day10-sample.txt", 13140)
  testDay10("part 1",        "day10.txt",        14320)

end Day10
