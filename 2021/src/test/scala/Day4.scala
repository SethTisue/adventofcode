class Day04 extends munit.FunSuite:

  type Board = List[Int]

  case class Input(numbers: List[Int], boards: List[Board])

  val lines = List(
    // rows
    List(0, 1, 2, 3, 4),
    List(5, 6, 7, 8, 9),
    List(10, 11, 12, 13, 14),
    List(15, 16, 17, 18, 19),
    List(20, 21, 22, 23, 24),
    // columns
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

  // part 1

  // TODO: this is hideously inefficient
  def play(input: Input): (Board, Int) =
    val wins =
      for segment <- input.numbers.inits.toList.reverse
          board <- input.boards
      yield
        val score = winningScore(board, segment)
        score.map((board, _))
    wins.flatten.head

  test("part 1 sample") {
    val input = readInput(io.Source.fromFile("day04-sample.txt").getLines)
    assertEquals(play(input)._2, 4512)
  }

  test("part 1 real") {
    val input = readInput(io.Source.fromFile("day04.txt").getLines)
    assertEquals(play(input)._2, 58412)
  }

  // part 2

  // TODO: this is hideously inefficient
  def play2(input: Input): Int =
    if input.boards.size == 1
    then
      play(input)._2
    else
      val board = play(input)._1
      play2(input.copy(boards = input.boards.filterNot(_ == board)))

  test("part 2 sample") {
    val input = readInput(io.Source.fromFile("day04-sample.txt").getLines)
    assertEquals(play2(input), 1924)
  }

  test("part 2 real") {
    val input = readInput(io.Source.fromFile("day04.txt").getLines)
    assertEquals(play2(input), 10030)
  }

end Day04
