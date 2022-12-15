class Day15 extends munit.FunSuite:

  def getInput(file: String): Iterator[String] =
    io.Source.fromResource(file)
      .getLines

  // part 1 tests

  def testDay15(name: String, file: String, expected: Int) =
    test(s"day 15 $name") {
      assertEquals(getInput(file).size, expected)
    }

  testDay15("part 1 sample", "day15-sample.txt",  26)
  // testDay15("part 1",        "day15.txt",       0)

end Day15
