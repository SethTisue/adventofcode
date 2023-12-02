class Day02 extends munit.FunSuite:

  // data model

  case class Game(n: Int, draws: List[Draw])
  case class Draw(red: Int, green: Int, blue: Int)

  // reading and parsing

  object Draw:
    def fromString(s: String): Draw =
      val pairs = s.split(',').map(_.trim).map:
          case s"$num $color" =>
            color -> num.toInt
        .toMap
      Draw(
        red = pairs.getOrElse("red", 0),
        green = pairs.getOrElse("green", 0),
        blue = pairs.getOrElse("blue", 0),
      )

  def getInput(name: String): List[Game] =
    io.Source.fromResource(name)
      .getLines.toList.map:
         case s"Game $n: $draws" =>
           Game(n.toInt, draws.split(';')
             .map(d => Draw.fromString(d.trim)).toList)

  // part 1

  def part1(name: String): Int =
    getInput(name)
      .filter: game =>
        game.draws.map(_.red).forall(_ <= 12) &&
          game.draws.map(_.green).forall(_ <= 13) &&
          game.draws.map(_.blue).forall(_ <= 14)
      .map(_.n)
      .sum

  test("part 1 sample"):
    assertEquals(8, part1("day02-sample.txt"))
  test("part 1"):
    assertEquals(2265, part1("day02.txt"))

  // part 2

  def part2(name: String): Int =
    getInput(name)
      .map: game =>
        game.draws.map(_.red).max *
          game.draws.map(_.green).max *
          game.draws.map(_.blue).max
      .sum

  test("part 2 sample"):
    assertEquals(2286, part2("day02-sample.txt"))
  test("part 2"):
    assertEquals(64097, part2("day02.txt"))

end Day02
