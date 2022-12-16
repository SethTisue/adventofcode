class Day15 extends munit.FunSuite:

  case class Sensor(sensorX: Int, sensorY: Int, beaconX: Int, beaconY: Int):
    def distance(x: Int, y: Int): Int =
      (x - sensorX).abs + (y - sensorY).abs
    def radius: Int =
      distance(beaconX, beaconY)

  def getInput(file: String): Iterator[Sensor] =
    io.Source.fromResource(file)
      .getLines
      .map {
        case s"Sensor at x=$x1, y=$y1: closest beacon is at x=$x2, y=$y2" =>
          Sensor(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
      }

  def countClearPositions(sensors: Iterable[Sensor], row: Int): Int =
    def cleared(s: Sensor): Range =
      val yDistance = (s.sensorY - row).abs
      (s.sensorX - s.radius + yDistance) to (s.sensorX + s.radius - yDistance)
    sensors.flatMap{s => val result = cleared(s); println(result); result}
      .toSet
      .diff(sensors.filter(_.beaconY == row).map(_.beaconX).toSet)
      .size

  // part 1 tests

  def testDay15(name: String, file: String, expected: Int) =
    test(s"day 15 $name") {
      val answer = countClearPositions(getInput(file).to(Iterable), row = 10)
      assertEquals(answer, expected)
    }

  testDay15("part 1 sample", "day15-sample.txt",  26)
  testDay15("part 1",        "day15.txt",       0)

end Day15
