import annotation.tailrec

class Day08 extends munit.FunSuite:

  /// data model

  enum Move:
    case Left, Right
  case class Node(left: String, right: String)
  type Network = Map[String, Node]

  def navigate(start: String, moves: Seq[Move], network: Network): LazyList[String] =
    def recurse(name: String, moves: LazyList[Move]): LazyList[String] =
      val next = moves.head match
        case Move.Left => network(name).left
        case Move.Right => network(name).right
      next #:: recurse(next, moves.tail)
    recurse(start, LazyList.continually(moves).flatten)

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
    navigate("AAA", moves, network).takeWhile(_ != "ZZZ").size + 1

  test("part 1 sample"):
    assertEquals(part1("day08-sample.txt"), 6)
  test("part 1"):
    assertEquals(part1("day08.txt"), 14681)

  /// part 2

  def part2(name: String): Int =
    val (moves, network) = getInput(name)
    @tailrec def recurse(routes: Seq[LazyList[String]], steps: Int): Int =
      if routes.map(_.head).forall(_.endsWith("Z"))
      then steps
      else recurse(routes.map(_.tail), steps + 1)
    recurse(network.keys.filter(_.endsWith("A")).toSeq.map(navigate(_, moves, network)), 1)

  test("part 2 sample"):
    assertEquals(part2("day08-sample2.txt"), 6)
  test("part 2"):
    assertEquals(part2("day08.txt"), -1)

end Day08
