import annotation.tailrec

class Day08 extends munit.FunSuite:

  /// data model

  enum Move:
    case Left, Right
  case class Node(left: String, right: String)
  type Network = Map[String, Node]

  def navigate(moves: Seq[Move], network: Network): LazyList[String] =
    def recurse(name: String, moves: LazyList[Move]): LazyList[String] =
      if name == "ZZZ"
      then LazyList()
      else
        val next = moves.head match
          case Move.Left => network(name).left
          case Move.Right => network(name).right
        next #:: recurse(next, moves.tail)
    recurse("AAA", LazyList.continually(moves).flatten)

  /// reading & parsing

  def getInput(name: String): (Seq[Move], Network) =
    val lines = io.Source.fromResource(name).getLines
    val moves = lines.next().map:
        case 'L' => Move.Left
        case 'R' => Move.Right
      .toSeq
    require(lines.next().isEmpty)
    val network = lines.map:
        case s"$name = ($left, $right)" =>
          name -> Node(left, right)
      .toMap
    (moves, network)

  /// part 1

  def part1(name: String): Int =
    val (moves, network) = getInput(name)
    navigate(moves, network).size

  test("part 1 sample"):
    assertEquals(part1("day08-sample.txt"), 6)
  test("part 1"):
    assertEquals(part1("day08.txt"), 14681)

/*
  /// part 2

  def part2(name: String): Int =
    0

  test("part 2 sample"):
    assertEquals(part2("day08-sample.txt"), 0)
  test("part 2"):
    assertEquals(part2("day08.txt"), 0)
 */

end Day08
