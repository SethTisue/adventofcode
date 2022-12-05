import util.chaining.*

class Day5 extends munit.FunSuite:

  // shared code

  type Stack = List[Char]

  case class Move(crates: Int, src: Int, dest: Int)
  object Move:
    def fromString(s: String): Move = s match
      case s"move $crates from $src to $dest" =>
        Move(crates.toInt, src.toInt, dest.toInt)

  // part 1

  def getInput(name: String): (List[Stack], List[Move]) =
    val lines =
      io.Source.fromResource(name)
        .getLines
        .toList
    val (stacksLines :+ _, _ +: movesLines) =
      lines.span(_.nonEmpty): @unchecked
    val stacks =
      stacksLines
        .map(_.toList.grouped(4).map(_.apply(1)).toList)
        .transpose.map(_.dropWhile(_ == ' '))
    (stacks, movesLines.map(Move.fromString))

  def solve(stacks: List[Stack], moves: List[Move]): String =
    moves match
      case Nil =>
        stacks.map(_.head).mkString
      case move :: more =>
        val newStacks =
          stacks
            .updated(move.src - 1, stacks(move.src - 1).drop(move.crates))
            .updated(move.dest - 1, stacks(move.src - 1).take(move.crates).reverse ::: stacks(move.dest - 1))
        solve(newStacks, more)

  // part 1 tests

  test("day 5 part 1 sample") {
    assertEquals("CMZ", Function.tupled(solve)(getInput("day5-sample.txt")))
  }

  test("day 5 part 1") {
    assertEquals("FRDSQRRCD", Function.tupled(solve)(getInput("day5.txt")))
  }

end Day5
