class Day14 extends munit.FunSuite:

  case class Point(x: Int, y: Int)

  type Path = List[Point]

  enum Square:
    case Rock, Empty, Sand

  type Grid = Array[Array[Square]]

  def newGrid(): Grid =
    Array.fill(1000)(Array.fill(200)(Square.Empty))

  def testDay14(name: String, file: String, expected: Int) =
    def getInput(file: String): Iterator[Path] =
      io.Source.fromResource(file)
        .getLines
        .map(pathFromString)
    def pathFromString(s: String): Path =
      s.split(" -> ")
        .map{case s"$x,$y" => Point(x.toInt, y.toInt)}
        .toList
    def pointsFromSegment(p1: Point, p2: Point): List[Point] =
      if p1.x == p2.x then
        (p1.y to p2.y by (p2.y - p1.y).sign).map(y => Point(p1.x, y)).toList
      else
        (p1.x to p2.x by (p2.x - p1.x).sign).map(x => Point(x, p1.y)).toList
    def gridFromPaths(paths: Iterator[Path]): Grid =
      val grid = newGrid()
      for path <- paths
          List(end1, end2) <- path.sliding(2)
          point <- pointsFromSegment(end1, end2)
      do
        grid(point.x)(point.y) = Square.Rock
      grid

    test(s"day 14 $name") {
      val grid = gridFromPaths(getInput(file))
      assertEquals(grid.flatten.count(_ == Square.Rock), expected)
    }

  testDay14("part 1 sample", "day14-sample.txt",  20)  // 24
  // testDay14("part 1",        "day14.txt",          0)

end Day14
