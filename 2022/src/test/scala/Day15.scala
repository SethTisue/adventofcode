class Day15 extends munit.FunSuite:

  // part 2 takes about 60 seconds
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  // shared code

  case class Sensor(x: Int, y: Int, beaconX: Int, beaconY: Int):
    val radius: Int =
      (beaconX - x).abs + (beaconY - y).abs

  def getInput(file: String): Iterator[Sensor] =
    io.Source.fromResource(file)
      .getLines
      .map {
        case s"Sensor at x=$x1, y=$y1: closest beacon is at x=$x2, y=$y2" =>
          Sensor(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
      }

  // part 1

  def countClearPositions(sensors: Iterable[Sensor], row: Int): Int =
    def cleared(s: Sensor): Range =
      val clearedRadius = s.radius - (s.y - row).abs
      (s.x - clearedRadius) to (s.x + clearedRadius)
    val beaconXs = sensors.filter(_.beaconY == row).map(_.beaconX).toSet
    sensors.flatMap(cleared)
      .toSet
      .diff(beaconXs)
      .size

  // part 2

  def findBeacon(sensors: Iterable[Sensor], bound: Int): (Int, Int) =
    def candidates(s: Sensor): Iterator[(Int, Int)] =
      for dx <- (0 to s.radius + 1).iterator
          point <- Iterator(
            (s.x + dx, s.y - s.radius - 1 + dx),
            (s.x - dx, s.y - s.radius - 1 + dx),
            (s.x + dx, s.y + s.radius + 1 - dx),
            (s.x - dx, s.y + s.radius + 1 - dx))
          if inBounds(point)
      yield point
    def isClear(x: Int, y: Int): Boolean =
      sensors.forall(s => (s.x - x).abs + (s.y - y).abs > s.radius)
    def inBounds(point: (Int, Int)): Boolean =
      point._1 >= 0 && point._2 >= 0 && point._1 <= bound && point._2 <= bound
    sensors.iterator.flatMap(candidates)
      .find(Function.tupled(isClear))
      .get

  // part 1 tests

  def testDay15Part1(name: String, file: String, row: Int, expected: Int) =
    test(s"day 15 $name") {
      val answer = countClearPositions(getInput(file).to(Iterable), row)
      assertEquals(answer, expected)
    }

  testDay15Part1("part 1 sample", "day15-sample.txt", row =      10,       26)
  testDay15Part1("part 1",        "day15.txt",        row = 2000000,  5144286)

  // part 2 tests

  def testDay15Part2(name: String, file: String, bound: Int, expected: Long) =
    test(s"day 15 $name") {
      val (x, y) = findBeacon(getInput(file).to(Iterable), bound)
      assertEquals(x * 4000000L + y, expected)
    }

  testDay15Part2("part 2 sample", "day15-sample.txt", bound =      20,       56000011L)
  testDay15Part2("part 2",        "day15.txt",        bound = 4000000, 10229191267339L)

end Day15
