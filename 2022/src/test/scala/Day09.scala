class Day09 extends munit.FunSuite:

  enum Direction(val dx: Int, val dy: Int):
    case U extends Direction( 0, -1)
    case D extends Direction( 0,  1)
    case L extends Direction(-1,  0)
    case R extends Direction( 1,  0)

  case class Location(x: Int, y: Int):
    def pullOne(follower: Location): Location =
      val (dx, dy) =
        (x - follower.x, y - follower.y) match
          case ( 2, dy) => ( 1,  dy.sign)
          case (-2, dy) => (-1,  dy.sign)
          case (dx,  2) => (dx.sign,   1)
          case (dx, -2) => (dx.sign,  -1)
          case _        => ( 0,   0)
      follower.copy(follower.x + dx, follower.y + dy)
    def pullAll(tail: List[Location]): List[Location] =
      if tail.isEmpty then Nil
      else
        val pulled = pullOne(tail.head)
        pulled :: pulled.pullAll(tail.tail)

  def run(moves: Iterator[Direction], tailSize: Int, debug: Boolean = false): Int =
    var head = Location(0, 0)
    var tail = List.fill(tailSize)(head)
    var visited = List(head)
    for move <- moves
    do
      if debug then println(move)
      head = head.copy(head.x + move.dx, head.y + move.dy)
      tail = head.pullAll(tail)
      visited ::= tail.last
      if debug then
        dumpBoard(head :: tail)
        println()
    if debug then
      println("FINAL:")
      dumpBoard(visited, showIndices = false)
    visited.distinct.size

  // useful for debugging

  def dumpBoard(rope: List[Location], showIndices: Boolean = true): Unit =
    val minX = rope.map(_.x).min.min(0)
    val maxX = rope.map(_.x).max.max(0)
    val minY = rope.map(_.y).min.min(0)
    val maxY = rope.map(_.y).max.max(0)
    for y <- minY to maxY do
      for x <- minX to maxX
      do
        val index = rope.indexOf(Location(x, y))
        print(index match
          case -1 => '.'
          case _ if !showIndices => '#'
          case 0 => 'H'
          case _ => ('0' + index).toChar
        )
      println

  // tests

  def testDay9(name: String, file: String, tailSize: Int, answer: Int) =
    test(s"day 9 $name") {
      val moves =
        io.Source.fromResource(file)
          .getLines
          .flatMap{case s"$dir $dist" =>
            List.fill(dist.toInt)(Direction.valueOf(dir))}
      assertEquals(run(moves, tailSize), answer)
    }

  testDay9("part 1 sample",   "day09-sample1.txt", 1, 13  )
  testDay9("part 1",          "day09.txt",         1, 5695)
  testDay9("part 2 sample 1", "day09-sample1.txt", 9, 1   )
  testDay9("part 2 sample 2", "day09-sample2.txt", 9, 36  )
  testDay9("part 2",          "day09.txt",         9, 2434)

end Day09
