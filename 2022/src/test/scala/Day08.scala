class Day08 extends munit.FunSuite:

  // shared

  type Forest = Vector[Vector[Int]]

  def allCoords(forest: Forest): IndexedSeq[(Int, Int)] =
    for y <- forest.indices
        x <- forest(y).indices
    yield (x, y)

  def viewCross(x: Int, y: Int, forest: Forest): List[IndexedSeq[Int]] =
    val left = (0 to (x - 1)).map(x0 => forest(x0)(y)).reverse
    val right = ((x + 1) until forest.size).map(x0 => forest(x0)(y))
    val up = (0 to (y - 1)).map(y0 => forest(x)(y0)).reverse
    val down = ((y + 1) until forest.size).map(y0 => forest(x)(y0))
    List(left, right, up, down)

  // part 1

  def visibleCount(forest: Forest): Int =
    def isVisible(x: Int, y: Int): Boolean =
      val height = forest(x)(y)
      viewCross(x, y, forest).exists(_.forall(_ < height))
    allCoords(forest).count(Function.tupled(isVisible))

  // part 2

  def scenic(x: Int, y: Int, forest: Forest): Int =
    val height = forest(x)(y)
    def viewingDistance(heights: IndexedSeq[Int]): Int =
      val shorter = heights.takeWhile(_ < height)
      if shorter.size == heights.size
      then heights.size
      else shorter.size + 1
    viewCross(x, y, forest).map(viewingDistance).product

  def mostScenic(forest: Forest): Int =
    allCoords(forest)
      .map((x, y) => scenic(x, y, forest))
      .max

  // tests

  def testDay8(name: String, file: String, solver: Forest => Int, answer: Int) =
    test(s"day 8 $name") {
      val forest =
        io.Source.fromResource(file)
          .getLines
          .map(_.map(_.toString.toInt).toVector)
          .toVector
      assertEquals(solver(forest), answer)
    }

  testDay8("part 1 sample", "day08-sample.txt", visibleCount, 21)
  testDay8("part 1",        "day08.txt",        visibleCount, 1776)
  testDay8("part 2 sample", "day08-sample.txt", mostScenic,   8)
  testDay8("part 2",        "day08.txt",        mostScenic,   234416)

end Day08
