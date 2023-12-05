class Day05 extends munit.FunSuite:

  /// data model

  case class Almanac(seeds: Vector[Long], mappings: Map[String, Mapping])
  case class Mapping(dest: String, ranges: Set[Range]):
   def lookup(n: Long): Long =
     ranges.flatMap(_.lookup(n)).headOption.getOrElse(n)
  case class Range(src: Long, length: Int, dest: Long):
    def lookup(n: Long): Option[Long] =
      if n >= src && n < src + length
      then
        // println(s"$n matches $this}: ${dest + n - src}")
        Some(dest + n - src)
      else None

  /// reading and parsing

  def getInput(name: String): Almanac =
    val contents = io.Source.fromResource(name)
      .getLines.mkString("\n")
    val sections = contents.split("\n\n")
    val seeds = sections.head match
      case s"seeds: $xs" =>
        xs.split(' ').map(_.toLong).toVector
    Almanac(seeds, sections.tail.map: section =>
      val lines: Array[String] = section.split('\n')
      lines.head match
        case s"$src-to-$dest map:" =>
          src -> Mapping(dest,
            lines.tail.map:
              case s"$dest $src $length" =>
                Range(src.toLong, length.toInt, dest.toLong)
            .toSet
          )
    .toMap)

  /// part 1

  def part1(name: String): Long =
    val almanac = getInput(name)
    // println(almanac)
    def seedToSoil(seed: Long): Long =
      almanac.mappings("seed").lookup(seed)
    def seedToLocation(seed: Long): Long =
      val result =
        almanac.mappings("humidity").lookup(
          almanac.mappings("temperature").lookup(
            almanac.mappings("light").lookup(
              almanac.mappings("water").lookup(
                almanac.mappings("fertilizer").lookup(
                  almanac.mappings("soil").lookup(
                    almanac.mappings("seed").lookup(seed)))))))
      result
    almanac.seeds.map(seedToLocation).min

  test("part 1 sample"):
    assertEquals(part1("day05-sample.txt"), 35L)
  test("part 1"):
    assertEquals(part1("day05.txt"), 0L)

/*
  /// part 2

  def part2(name: String): Int =
    0

  test("part 2 sample"):
    assertEquals(part2("day05-sample.txt"), 0)
  test("part 2"):
    assertEquals(part2("day05.txt"), 0)
 */

end Day05
