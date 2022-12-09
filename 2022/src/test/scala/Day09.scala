class Day09 extends munit.FunSuite:

  enum Direction:
    case Up, Down, Left, Right
  object Direction:
    def fromChar(c: Char): Direction =
      c match
        case 'U' => Up
        case 'D' => Down
        case 'L' => Left
        case 'R' => Right

  case class Command(direction: Direction, distance: Int)
  object Command:
    def fromString(s: String): Command =
      s match
       case s"$dir $dist" =>
         Command(Direction.fromChar(dir(0)), dist.toInt)

  case class Location(x: Int, y: Int)

  def run(commands: Seq[Command]): Int = 13

  // tests

  def testDay9(name: String, file: String, answer: Int) =
    test(s"day 9 $name") {
      val commands =
        io.Source.fromResource(file)
          .getLines
          .map(Command.fromString)
          .toList
      assertEquals(run(commands), answer)
    }

  testDay9("part 1 sample", "day09-sample.txt", 13)
  // testDay9("part 1",        "day09.txt",        1776)

end Day09
