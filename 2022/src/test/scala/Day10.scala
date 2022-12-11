class Day10 extends munit.FunSuite:

  // shared code

  def getInput(file: String): Iterator[Int] =
    io.Source.fromResource(file).getLines
      .flatMap {
        case "noop"          => List(0)
        case s"addx $amount" => List(0, amount.toInt)
      }

  def xHistory(deltas: Iterator[Int]): Iterator[Int] =
    deltas.scanLeft(1)(_ + _)

  // part 1

  def signalStrengthSum(deltas: Iterator[Int]): Int =
    val xs = xHistory(deltas).to(IndexedSeq)
    (20 to xs.size by 40)
      .map(n => xs(n - 1) * n)
      .sum

  // part 2

  def imageFrom(deltas: Iterator[Int]): String =
    val xs = xHistory(deltas)
    xs.zipWithIndex.map((x, index) => if (x - (index % 40)).abs <= 1 then '#' else '.')
      .grouped(40).map(_.mkString).filter(_.size == 40).mkString("\n")

  // part 1 tests

  def testDay10Part1(name: String, file: String, expected: Int) =
    test(s"day 10 $name") {
      assertEquals(signalStrengthSum(getInput(file)), expected)
    }

  testDay10Part1("part 1 sample", "day10-sample.txt", 13140)
  testDay10Part1("part 1",        "day10.txt",        14320)

  // part 2 tests

  def testDay10Part2(name: String, file: String, expected: String) =
    test(s"day 10 $name") {
      assertEquals(imageFrom(getInput(file)), expected)
    }

  testDay10Part2("part 2 sample", "day10-sample.txt",
    """|##..##..##..##..##..##..##..##..##..##..
       |###...###...###...###...###...###...###.
       |####....####....####....####....####....
       |#####.....#####.....#####.....#####.....
       |######......######......######......####
       |#######.......#######.......#######.....""".stripMargin)
  testDay10Part2("part 2", "day10.txt",
    """|###...##..###..###..#..#..##..###....##.
       |#..#.#..#.#..#.#..#.#.#..#..#.#..#....#.
       |#..#.#....#..#.###..##...#..#.#..#....#.
       |###..#....###..#..#.#.#..####.###.....#.
       |#....#..#.#....#..#.#.#..#..#.#....#..#.
       |#.....##..#....###..#..#.#..#.#.....##..""".stripMargin)

end Day10
