class Day4 extends munit.FunSuite:

  type Board = List[Int]

  case class Input(numbers: List[Int], boards: List[Board])

  val lines = List(
    List(0, 1, 2, 3, 4),
    List(5, 6, 7, 8, 9),
    List(10, 11, 12, 13, 14),
    List(15, 16, 17, 18, 19),
    List(20, 21, 22, 23, 24),
    List(0, 5, 10, 15, 20),
    List(1, 6, 11, 16, 21),
    List(2, 7, 12, 17, 22),
    List(3, 8, 13, 18, 23),
    List(4, 9, 14, 19, 24),
  )

  def winningScore(board: Board, numbers: List[Int]): Option[Int] =
    if lines.exists(_.forall(index => numbers.contains(board(index))))
    then Some(board.diff(numbers).sum * numbers.last)
    else None

  def play(input: Input): Int =
    val wins =
      for segment <- input.numbers.inits.toList.reverse
          board <- input.boards
      yield winningScore(board, segment)
    println(wins)
    wins.flatten.head

  // TODO I should use cats-parse for this

  def readInput(iter: Iterator[String]): Input =
    val numbers = iter.next().split(',').map(_.toInt).toList
    def parse(line: String): List[Int] =
      line.trim.split("\\s+").map(_.toInt).toList.ensuring(_.size == 5)
    val boards = collection.mutable.ListBuffer[Board]()
    while iter.hasNext do
      val next = iter.next()
      if next.trim.nonEmpty then
        boards.addOne(parse(next) ++ Iterator.continually(parse(iter.next())).take(4).flatten.toList)
    Input(numbers, boards.toList.ensuring(_.forall(_.size == 25)))

  test("sample") {
    val input = readInput(io.Source.fromFile("day4-sample.txt").getLines)
    assertEquals(input.numbers.size, 27)
    assertEquals(input.boards.size, 3)
    assertEquals(play(input), 4512)
  }

  test("real") {
    val input = readInput(io.Source.fromFile("day4.txt").getLines)
    assertEquals(play(input), 58412)
  }

end Day4
