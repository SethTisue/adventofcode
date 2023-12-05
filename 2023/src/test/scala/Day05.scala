class Day05 extends munit.FunSuite:

  /// data model

  case class Almanac(seeds: Vector[Long], mappings: Map[String, Mapping]):
    def lookup(seed: Long): Long =
      mappings("humidity").lookup(
        mappings("temperature").lookup(
          mappings("light").lookup(
            mappings("water").lookup(
              mappings("fertilizer").lookup(
                mappings("soil").lookup(
                  mappings("seed").lookup(seed)))))))
  case class Mapping(dest: String, ranges: Seq[Range]):
   def lookup(n: Long): Long =
     ranges.collectFirst:
       case range if range.lookup(n) != -1 =>
         range.lookup(n)
     .getOrElse(n)
  case class Range(src: Long, length: Int, dest: Long):
    def lookup(n: Long): Long =
      if n >= src && n < src + length
      then dest + n - src
      else -1L

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
            .toSeq.sortBy(_.src))
    .toMap)

  /// part 1

  def part1(name: String): Long =
    val almanac = getInput(name)
    almanac.seeds.map(almanac.lookup).min

  test("part 1 sample"):
    assertEquals(part1("day05-sample.txt"), 35L)
  test("part 1"):
    assertEquals(part1("day05.txt"), 662197086L)

  /// part 2

  def part2(name: String): Long =
    val almanac = getInput(name)
    val locs: Iterator[Long] =
      for case Vector(start, length) <- almanac.seeds.grouped(2)
          _ = println(start)
          seed <- start until (start + length)
      yield almanac.lookup(seed)
    locs.min

  test("part 2 sample"):
    assertEquals(part2("day05-sample.txt"), 46L)
  test("part 2"):
    assertEquals(part2("day05.txt"), 0L)

end Day05
