import util.chaining.*

class Day5 extends munit.FunSuite:

  type Stack = List[Char]

  // 0-indexed, even though input is 1-indexed
  case class Move(crates: Int, src: Int, dest: Int)
  object Move:
    def fromString(s: String): Move = s match
      case s"move $crates from $src to $dest" =>
        Move(crates.toInt, src.toInt - 1, dest.toInt - 1)

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

  def solve(stacks: List[Stack], moves: List[Move], withReversing: Boolean): String =
    moves match
      case Nil =>
        stacks.map(_.head).mkString
      case move :: more =>
        val cratesMoved =
          stacks(move.src)
            .take(move.crates)
            .pipe(if withReversing then _.reverse else identity)
        val newStacks =
          stacks
            .updated(move.src, stacks(move.src).drop(move.crates))
            .updated(move.dest, cratesMoved ::: stacks(move.dest))
        solve(newStacks, more, withReversing)

  // part 1 tests

  test("day 5 part 1 sample") {
    val (stacks, moves) = getInput("day5-sample.txt")
    assertEquals("CMZ", solve(stacks, moves, withReversing = true))
  }

  test("day 5 part 1") {
    val (stacks, moves) = getInput("day5.txt")
    assertEquals("FRDSQRRCD", solve(stacks, moves, withReversing = true))
  }

  // part 2 tests

  test("day 5 part 2 sample") {
    val (stacks, moves) = getInput("day5-sample.txt")
    assertEquals("MCD", solve(stacks, moves, withReversing = false))
  }

  test("day 5 part 2") {
    val (stacks, moves) = getInput("day5.txt")
    assertEquals("HRFTQVWNN", solve(stacks, moves, withReversing = false))
  }

end Day5
