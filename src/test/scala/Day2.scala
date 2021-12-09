class Day2 extends munit.FunSuite:

  enum Command:
    case Forward(n: Int)
    case Up(n: Int)
    case Down(n: Int)

  import Command.*

  case class Position1(horiz: Int, depth: Int)

  def parse(input: String): Command =
    input match
      case s"forward $n" => Forward(n.toInt)
      case s"up $n"      => Up(n.toInt)
      case s"down $n"    => Down(n.toInt)

  val sample =
    List("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")
      .map(parse)

  // Part 1

  def move1(p: Position1, c: Command): Position1 =
    c match
      case Forward(n) => p.copy(horiz = p.horiz + n)
      case Down(n)    => p.copy(depth = p.depth + n)
      case Up(n)      => p.copy(depth = p.depth - n)

  def navigate1(cmds: IterableOnce[Command]): Position1 =
    val initial = Position1(0, 0)
    cmds.foldLeft(initial)(move1)

  test("part 1 sample") {
    val finalPos = navigate1(sample)
    assertEquals(finalPos.horiz * finalPos.depth, 150)
  }

  test("part 1 real") {
    val input = io.Source.fromFile("day2.txt").getLines.map(parse)
    val finalPos = navigate1(input)
    assertEquals(finalPos.horiz * finalPos.depth, 1636725)
  }

  // Part 2

  case class Position2(horiz: Int, depth: Int, aim: Int)

  def move2(p: Position2, c: Command): Position2 =
    c match
      case Forward(n) => p.copy(horiz = p.horiz + n, depth = p.depth + p.aim * n)
      case Down(n)    => p.copy(aim = p.aim + n)
      case Up(n)      => p.copy(aim = p.aim - n)

  def navigate2(cmds: IterableOnce[Command]): Position2 =
    val initial = Position2(0, 0, 0)
    cmds.foldLeft(initial)(move2)

  test("part 2 sample") {
    val finalPos = navigate2(sample)
    assertEquals(finalPos.horiz * finalPos.depth, 900)
  }

  test("part 2 real") {
    val initial = Position2(0, 0, 0)
    val input = io.Source.fromFile("day2.txt").getLines.map(parse)
    val finalPos = navigate2(input)
    assertEquals(finalPos.horiz * finalPos.depth, 1872757425)
  }

end Day2
