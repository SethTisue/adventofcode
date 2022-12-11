class Day10 extends munit.FunSuite:

  enum Instruction:
    case Noop
    case AddX(amount: Int)

  import Instruction.*

  case class State(x: Int, cycle: Int)

  val initialState = State(x = 1, cycle = 1)

  def run(state: State, instruction: Instruction): State =
    instruction match
      case Noop =>
        state.copy(cycle = state.cycle + 1)
      case AddX(amount) =>
        state.copy(x = state.x + amount, cycle = state.cycle + 1)

  // tests

  def testDay10(name: String, file: String, expected: Int) =
    test(s"day 10 $name") {
      val input: Iterator[Instruction] =
        io.Source.fromResource(file)
          .getLines
          .flatMap{
            case "noop" =>
              List(Noop)
            case s"addx $amount" =>
              List(Noop, AddX(amount.toInt))
          }
      val states = input.to(LazyList).scanLeft(initialState)(run)
//      states.foreach(println)
      import util.chaining.*
      val result =
        (19 until states.size by 40)
          .map(states.apply)
//          .tapEach(println)
          .map(s => s.cycle * s.x)
//          .tapEach(println)
          .sum
      assertEquals(result, expected)
    }

  testDay10("part 1 sample", "day10-sample.txt", 13140)
  testDay10("part 1",        "day10.txt",        14320)

end Day10
