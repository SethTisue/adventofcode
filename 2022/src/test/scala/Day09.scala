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

  case class Location(x: Int, y: Int):
    def move(dir: Direction): Location =
      dir match
        case Direction.Up    => copy(x, y - 1)
        case Direction.Down  => copy(x, y + 1)
        case Direction.Left  => copy(x - 1, y)
        case Direction.Right => copy(x + 1, y)
    def moveTail(tail: Location): Location =
      val (dx, dy) =
        (x - tail.x, y - tail.y) match
          case ( 2, dy) => ( 1,  dy)
          case (-2, dy) => (-1,  dy)
          case (dx,  2) => (dx,   1)
          case (dx, -2) => (dx,  -1)
          case _        => ( 0,   0)
      tail.copy(tail.x + dx, tail.y + dy)

  def run(commands: Seq[Command]): Int =
    var head = Location(0, 0)
    var tail = head
    var visited = List(tail)
    for Command(direction, distance) <- commands
        _ <- 0 until distance
    do
      head = head.move(direction)
      tail = head.moveTail(tail)
      visited ::= tail
    visited.distinct.size

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
  testDay9("part 1",        "day09.txt",        5695)

end Day09
