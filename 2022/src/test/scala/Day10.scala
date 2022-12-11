class Day10 extends munit.FunSuite:

  case class State(x: Int)

  val initialState = State(x = 1)

  def run(state: State, delta: Int): State =
    state.copy(x = state.x + delta)

  // tests

  def testDay10(name: String, file: String, expected: Int) =
    test(s"day 10 $name") {
      val input: Iterator[Int] =
        io.Source.fromResource(file)
          .getLines
          .flatMap{
            case "noop" =>
              List(0)
            case s"addx $amount" =>
              List(0, amount.toInt)
          }
      val states = input.to(LazyList).scanLeft(initialState)(run)
      val result =
        (20 until states.size by 40)
          .map(n => states(n - 1).x * n)
          .sum
      assertEquals(result, expected)
    }

  testDay10("part 1 sample", "day10-sample.txt", 13140)
  testDay10("part 1",        "day10.txt",        14320)

end Day10
