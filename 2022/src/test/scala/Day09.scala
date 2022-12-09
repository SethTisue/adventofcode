class Day09 extends munit.FunSuite:

  // location and movement logic

  enum Direction(val dx: Int, val dy: Int):
    case U extends Direction( 0, -1)
    case D extends Direction( 0,  1)
    case L extends Direction(-1,  0)
    case R extends Direction( 1,  0)

  case class Location(x: Int, y: Int)

  val origin = Location(0, 0)

  def pullOne(first: Location, follower: Location): Location =
    val (dx, dy) = (first.x - follower.x, first.y - follower.y)
    if dx.abs == 2 || dy.abs == 2 then
      follower.copy(follower.x + dx.sign, follower.y + dy.sign)
    else
      follower

  def pullAll(head: Location, tail: List[Location]): List[Location] =
    if tail.isEmpty then Nil
    else
      val pulled = pullOne(head, tail.head)
      pulled :: pullAll(pulled, tail.tail)

  // state management

  case class State(head: Location, tail: List[Location], visited: Set[Location])

  def initialState(tailSize: Int) =
    State(origin, List.fill(tailSize)(origin), Set(origin))

  def nextState(state: State, move: Direction): State =
    val newHead = Location(state.head.x + move.dx, state.head.y + move.dy)
    val newTail = pullAll(newHead, state.tail)
    State(
      head = newHead,
      tail = newTail,
      visited = state.visited + newTail.last)

  def finalState(tailSize: Int, moves: Seq[Direction]): State =
    moves.foldLeft(initialState(tailSize))(nextState)

  // tests

  def testDay9(name: String, file: String, tailSize: Int, answer: Int) =
    test(s"day 9 $name") {
      val moves: Seq[Direction] =
        io.Source.fromResource(file)
          .getLines
          .flatMap{
            case s"$dir $dist" =>
              List.fill(dist.toInt)(Direction.valueOf(dir))
          }.toSeq
      assertEquals(finalState(tailSize, moves).visited.size, answer)
    }

  testDay9("part 1 sample",   "day09-sample1.txt", 1, 13  )
  testDay9("part 1",          "day09.txt",         1, 5695)
  testDay9("part 2 sample 1", "day09-sample1.txt", 9, 1   )
  testDay9("part 2 sample 2", "day09-sample2.txt", 9, 36  )
  testDay9("part 2",          "day09.txt",         9, 2434)

end Day09
