class Day14 extends munit.FunSuite:

  // data

  case class Point(x: Int, y: Int)

  type Path = List[Point]

  enum Square:
    case Rock, Empty, Sand

  type Grid = Array[Array[Square]]

  // logic

  def dropSand(grid: Grid, loc: Point): Boolean = // true if the sand stopped
    val newY = loc.y + 1
    newY < 200 && {
      val candidates = List(loc.x, loc.x - 1, loc.x + 1)
      candidates.find(newX => grid(newX)(newY) == Square.Empty) match
        case Some(newX) =>
          dropSand(grid, Point(newX, newY))
        case None =>
          grid(loc.x)(loc.y) = Square.Sand
          true
    }

  // reading the input

  def getInput(file: String): Grid =
    def pathFromString(s: String): Path =
      s.split(" -> ")
        .map{case s"$x,$y" => Point(x.toInt, y.toInt)}
        .toList
    def pointsFromSegment(p1: Point, p2: Point): List[Point] =
      if p1.x == p2.x then
        (p1.y to p2.y by (p2.y - p1.y).sign)
          .map(y => Point(p1.x, y)).toList
      else
        (p1.x to p2.x by (p2.x - p1.x).sign)
          .map(x => Point(x, p1.y)).toList
    def gridFromPaths(paths: Iterator[Path]): Grid =
      val grid = Array.fill(1000)(Array.fill(200)(Square.Empty))
      for path <- paths
          List(end1, end2) <- path.sliding(2)
          point <- pointsFromSegment(end1, end2)
      do grid(point.x)(point.y) = Square.Rock
      grid
    gridFromPaths(
      io.Source.fromResource(file)
        .getLines
       .map(pathFromString))

  // part 1 tests

  def testDay14(name: String, file: String, expected: Int) =
    test(s"day 14 $name") {
      val grid = getInput(file)
      assertEquals(
        Iterator.continually(dropSand(grid, Point(500, 0))).takeWhile(_ == true).size,
        expected)
    }

  testDay14("part 1 sample", "day14-sample.txt",  24)
  testDay14("part 1",        "day14.txt",        964)

end Day14
