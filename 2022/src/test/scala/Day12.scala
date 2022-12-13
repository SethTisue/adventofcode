// This code is really awful but I don't feel like cleaning it up.
// I just wanted to solve this as fast as possible and move on.

class Day12 extends munit.FunSuite:

  def getInput(file: String): Vector[Vector[Char]] =
    io.Source.fromResource(file)
      .getLines
      .map(_.to(Vector))
      .to(Vector)
      .transpose

  def solve(grid: Vector[Vector[Char]], initialX: Int, initialY: Int, maxX: Int, maxY: Int, uphill: Boolean): Int =
    val distances: Array[Array[Int]] =
      Array.fill(maxX)(Array.fill(maxY)(-1))
    var queue = Vector((initialX, initialY, 0))
    distances(initialX)(initialY) = 0
    val endDest = if uphill then 'E' else 'a'
    while queue.nonEmpty do
      val here = (queue.head._1, queue.head._2)
      val steps = (queue.head._3)
      distances(here._1)(here._2) = steps
      queue = queue.tail
      def reachable(src: Char, dest: Char) =
        val effectiveSrc = if src == 'S' then 'a' else if src == 'E' then 'z' else src
        val effectiveDest = if dest == 'E' then 'z' else if dest == 'S' then 'a' else dest
        if uphill then effectiveDest - effectiveSrc <= 1
        else effectiveDest - effectiveSrc >= -1
      def next(dx: Int)(dy: Int): Boolean =
        val valueHere = grid(here._1)(here._2)
        val newX = here._1 + dx
        val newY = here._2 + dy
        if newX >= 0 && newX < maxX && newY >= 0 && newY < maxY then
          if reachable(src = valueHere, dest = grid(newX)(newY)) && grid(newX)(newY) == endDest then
            return true
          if distances(newX)(newY) == -1 then
            val dest = grid(newX)(newY)
            if reachable(src = valueHere, dest = grid(newX)(newY)) && !queue.exists(entry => entry._1 == newX && entry._2 == newY) then
              queue :+= (newX, newY, steps + 1)
        false
      if next(1)(0) then return steps + 1
      if next(0)(1) then return steps + 1
      if next(0)(-1) then return steps + 1
      if next(-1)(0) then return steps + 1
    throw new IllegalStateException

  def testDay12(name: String, file: String, uphill: Boolean, expected: Int) =
    test(s"day 12 $name") {
      val grid = getInput(file)
      val start = if uphill then 'S' else 'E'
      val initialX = grid.indexWhere(_.contains(start))
      val initialY = grid(initialX).indexOf(start)
      assertEquals(
        solve(grid, maxX = grid.size, maxY = grid.head.size,
          initialX = initialX, initialY = initialY,
          uphill = uphill),
        expected)
    }

  testDay12("part 1 sample", "day12-sample.txt",  true,   31)
  testDay12("part 1",        "day12.txt",         true,  481)

  testDay12("part 2 sample", "day12-sample.txt",  false,  29)
  testDay12("part 2",        "day12.txt",         false, 480)

end Day12
