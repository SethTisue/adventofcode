class Day12 extends munit.FunSuite:

  def getInput(file: String): Vector[Vector[Char]] =
    io.Source.fromResource(file)
      .getLines
      .map(_.to(Vector))
      .to(Vector)
      .transpose

  // this is some craptastic code but I want to wait to clean it up
  // until I've seen what part 2 needs

  def solve(grid: Vector[Vector[Char]], initialY: Int, maxX: Int, maxY: Int): Int =
    val distances: Array[Array[Int]] =
      Array.fill(maxX)(Array.fill(maxY)(-1))
    var queue = Vector((0, initialY, 0))  // reaching (0,initialY) takes 0 steps
    require(grid(0)(initialY) == 'S')
    distances(0)(initialY) = 0
    while queue.nonEmpty do
      val here = (queue.head._1, queue.head._2)
      val steps = (queue.head._3)
      distances(here._1)(here._2) = steps
      queue = queue.tail
      def next(dx: Int)(dy: Int): Boolean =
        if here._1 + dx >= 0 && here._1 + dx < maxX && here._2 + dy >= 0 && here._2 + dy < maxY then
          if (grid(here._1)(here._2) == 'z' || grid(here._1)(here._2) == 'y')&& grid(here._1 + dx)(here._2 + dy) == 'E' then
            return true
          if distances(here._1 + dx)(here._2 + dy) == -1 then
            if (grid(here._1)(here._2) == 'S' || grid(here._1 + dx)(here._2 + dy) <= grid(here._1)(here._2) + 1) && !queue.exists(entry => entry._1 == here._1 + dx && entry._2 == here._2 + dy) then
              queue :+= (here._1 + dx, here._2 + dy, steps + 1)
        false
      if next(1)(0) then return steps + 1
      if next(0)(1) then return steps + 1
      if next(0)(-1) then return steps + 1
      if next(-1)(0) then return steps + 1
    throw new IllegalStateException

  def testDay12(name: String, file: String, expected: Int) =
    test(s"day 12 $name") {
      val grid = getInput(file)
      assertEquals(solve(grid, maxX = grid.size, maxY = grid.head.size, initialY = grid.head.indexOf('S')), expected)
    }

  testDay12("part 1 sample", "day12-sample.txt",  31)
  testDay12("part 1",        "day12.txt",        481)

end Day12
