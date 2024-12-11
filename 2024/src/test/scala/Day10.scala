class Day10 extends munit.FunSuite:

  /// shared logic

  type Pos = (Int, Int)
  extension (pos: Pos)
    def +(other: Pos): Pos =
      (pos._1 + other._1, pos._2 + other._2)

  type Topo = Vector[Vector[Int]]
  extension (topo: Topo)
    def apply(pos: Pos): Int =
      topo(pos._1)(pos._2)
    def inBounds(pos: Pos): Boolean =
      pos._1 >= 0 && pos._1 < topo.size &&
      pos._2 >= 0 && pos._2 < topo.head.size
    def positions =
      for row <- topo.indices
          column <- topo.head.indices
      yield (row, column)

  type Graph = Map[Pos, Set[Pos]]

  /// reading & parsing

  def getInput(name: String): Topo =
    io.Source.fromResource(name)
    .getLines
    .map(_.map(_ - '0').toVector)
    .toVector

  /// part 1

  def computeGraph(topo: Topo): Graph =
    def reachables(pos: Pos): Set[Pos] =
      Set((-1, 0), (1, 0), (0, -1), (0, 1))
      .flatMap: offsets =>
        Some(pos + offsets)
        .filter: nextPos =>
          topo.inBounds(nextPos) && topo(nextPos) == topo(pos) + 1
    topo.positions
    .map(pos => pos -> reachables(pos))
    .toMap

  // note that we don't bother memoizing, which means we're doing some
  // redundant computation, but it turns out not to run plenty fast
  // anyway on the size of input that we have

  def solve1(topo: Topo): Int =
    val graph = computeGraph(topo)
    def reachableSummits(pos: Pos): Set[Pos] =
      if topo(pos) == 9
      then Set(pos)
      else graph(pos).flatMap(reachableSummits)
    topo.positions
    .filter(pos => topo(pos) == 0)
    .map(pos => reachableSummits(pos).size)
    .sum

  def part1(name: String): Int =
    solve1(getInput(name))

  test("part 1 sample"):
    assertEquals(part1("day10-sample.txt"), 36)
  test("part 1"):
    assertEquals(part1("day10.txt"), 512)

  /// part 2

  // this is very nearly the same code as part 1. the only real difference
  // is that we no longer need to keep track of the endpoint of each hiking path,
  // since we no longer want to discard multiple paths with the same endpoint

  def solve2(topo: Topo): Int =
    val graph = computeGraph(topo)
    def routes(pos: Pos): Int =
      if topo(pos) == 9
      then 1
      else graph(pos).toSeq.map(routes).sum
    topo.positions
    .filter(pos => topo(pos) == 0)
    .map(routes)
    .sum

  def part2(name: String): Int =
    solve2(getInput(name))

  test("part 2 sample"):
    assertEquals(part2("day10-sample.txt"), 81)
  test("part 2"):
    assertEquals(part2("day10.txt"), 1045)

end Day10
