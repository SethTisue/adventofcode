class Day08 extends munit.FunSuite:

  // shared

  type Forest = Vector[Vector[Int]]

  def getInput(name: String): Forest =
    io.Source.fromResource(name)
      .getLines
      .map(_.map(_.toString.toInt).toVector)
      .toVector

  def allTrees(forest: Forest): Vector[(Int, Int)] =
    for y <- forest.indices.toVector
        x <- forest(y).indices
    yield (x, y)

  // part 1

  def isVisible(x: Int, y: Int, forest: Forest): Boolean =
    val height = forest(x)(y)
    (0 to (x - 1)).forall(x0 => forest(x0)(y) < height) ||
      ((x + 1) until forest.size).forall(x0 => forest(x0)(y) < height) ||
      (0 to (y - 1)).forall(y0 => forest(x)(y0) < height) ||
      ((y + 1) until forest.size).forall(y0 => forest(x)(y0) < height)

  def visibleCount(forest: Forest): Int =
    allTrees(forest).count((x, y) => isVisible(x, y, forest))

  // part 2

  def scenic(x: Int, y: Int, forest: Forest): Int =
    val left = (0 to (x - 1)).map(x0 => forest(x0)(y)).reverse
    val right = ((x + 1) until forest.size).map(x0 => forest(x0)(y))
    val up = (0 to (y - 1)).map(y0 => forest(x)(y0)).reverse
    val down = ((y + 1) until forest.size).map(y0 => forest(x)(y0))
    val height = forest(x)(y)
    def visible(heights: IndexedSeq[Int]): Int =
      if heights.exists(_ >= height)
      then heights.takeWhile(_ < height).size + 1
      else heights.size
    List(left, right, up, down).map(visible).product

  def mostScenic(forest: Forest): Int =
    allTrees(forest)
      .map((x, y) => scenic(x, y, forest))
      .max

  // tests

  def testDay8(name: String, file: String, solver: Forest => Int, answer: Int) =
    test(s"day 8 $name") {
      assertEquals(solver(getInput(file)), answer)
    }

  testDay8("part 1 sample", "day08-sample.txt", visibleCount, 21)
  testDay8("part 1",        "day08.txt",        visibleCount, 1776)
  testDay8("part 2 sample", "day08-sample.txt", mostScenic,   8)
  testDay8("part 2",        "day08.txt",        mostScenic,   234416)

end Day08
