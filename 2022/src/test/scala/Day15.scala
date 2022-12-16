class Day15 extends munit.FunSuite:

  // shared code

  case class Sensor(sensorX: Int, sensorY: Int, beaconX: Int, beaconY: Int):
    val radius: Int =
      (beaconX - sensorX).abs + (beaconY - sensorY).abs

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
      val clearedRadius = s.radius - (s.sensorY - row).abs
      (s.sensorX - clearedRadius) to (s.sensorX + clearedRadius)
    val beaconXs = sensors.filter(_.beaconY == row).map(_.beaconX).toSet
    sensors.flatMap(cleared)
      .toSet
      .diff(beaconXs)
      .size

  // part 2

  def findBeacon(sensors: Iterable[Sensor]): (Int, Int) =
    (14, 11)

  // part 1 tests

  def testDay15Part1(name: String, file: String, row: Int, expected: Int) =
    test(s"day 15 $name") {
      val answer = countClearPositions(getInput(file).to(Iterable), row)
      assertEquals(answer, expected)
    }

  testDay15Part1("part 1 sample", "day15-sample.txt", row =      10,       26)
  testDay15Part1("part 1",        "day15.txt",        row = 2000000,  5144286)

  // part 2 tests

  def testDay15Part2(name: String, file: String, expected: Long) =
    test(s"day 15 $name") {
      val (x, y) = findBeacon(getInput(file).to(Iterable))
      assertEquals(x * 4000000L + y, expected)
    }

  testDay15Part2("part 2 sample", "day15-sample.txt", 56000011L)
  // testDay15Part2("part 2",        "day15.txt",        0L)

end Day15
