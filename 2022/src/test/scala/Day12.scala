class Day12 extends munit.FunSuite:

  def getInput(file: String): Vector[Vector[Char]] =
    io.Source.fromResource(file)
      .getLines
      .map(_.to(Vector))
      .to(Vector)

  def testDay12(name: String, file: String, expected: Int) =
    test(s"day 12 $name") {
      val grid = getInput(file)
      assertEquals(grid.map(_.size).sum, expected)
    }

  testDay12("part 1 sample", "day12-sample.txt", 31)

end Day12
