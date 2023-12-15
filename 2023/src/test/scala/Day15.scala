class Day15 extends munit.FunSuite:

  /// reading & parsing

  def getInput(name: String): Seq[String] =
    io.Source.fromResource(name)
      .getLines
      .flatMap(_.split(','))
      .toVector

  /// part 1 core logic

  def hash(s: String): Int =
    s.foldLeft(0): (acc, c) =>
      (acc + c) * 17 % 256

  /// part 2 core logic

  type State = IndexedSeq[Box]
  type Box = IndexedSeq[Lens]
  case class Lens(label: String, focalLength: Int)
  enum Instruction:
    case Remove(label: String)
    case Put(label: String, focalLength: Int)
  object Instruction:
    def fromString(s: String): Instruction =
      s match
        case s"$s=$n" => Instruction.Put(s, n.toInt)
        case s"$s-" => Instruction.Remove(s)
        case _ => throw new RuntimeException(s"invalid input: $s")

  /// part 1 driver and tests

  def part1(name: String): Int =
    getInput(name).map(hash(_)).sum

  test("part 1 sample"):
    assertEquals(part1("day15-sample.txt"), 1320)
  test("part 1"):
    assertEquals(part1("day15.txt"), 504449)

  /// part 2 driver and tests

  def update(state: State, instr: Instruction): State =
    val label = instr match
      case Instruction.Remove(label) => label
      case Instruction.Put(label, _) => label
    val boxNumber = hash(label)
    val box = state(boxNumber)
    val idx = box.indexWhere(_.label == label)
    state.updated(boxNumber,
      instr match
        case Instruction.Remove(label) =>
          idx match
            case -1 =>
              box
            case idx =>
              box.patch(idx, Nil, 1)
        case Instruction.Put(label, focalLength) =>
          idx match
            case -1 =>
              box :+ Lens(label, focalLength)
            case idx =>
              box.updated(idx, Lens(label, focalLength))
    )

  def focusingPower(state: State): Int =
    state.indices.map: boxNumber =>
      (boxNumber + 1) * state(boxNumber).zipWithIndex.map: (lens, idx) =>
          (idx + 1) * lens.focalLength
        .sum
    .sum

  import util.chaining.*

  def part2(name: String): Int =
    getInput(name)
      .map(Instruction.fromString)
      .foldLeft(Vector.fill(256)(Vector()))(update)
      .pipe(focusingPower)

  test("part 2 sample"):
    assertEquals(part2("day15-sample.txt"), 145)
  test("part 2"):
    assertEquals(part2("day15.txt"), 262044)

end Day15
