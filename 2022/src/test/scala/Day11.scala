class Day11 extends munit.FunSuite:

  case class Monkey(
    var items: List[Long],
    var inspected: Int = 0,
    operation: Long => Long,
    divisibleBy: Int,
    trueMonkey: Int,
    falseMonkey: Int)

  object Monkey:
    def parseOperation(s: String): Long => Long =
      s match
        case s"$left $middle $right" =>
          n =>
            val op1 = if left  == "old" then n else left.toLong
            val op2 = if right == "old" then n else right.toLong
            middle match
              case "+" => op1 + op2
              case "*" => op1 * op2
    def fromString(lines: Array[String]): Monkey =
      Monkey(
        items = lines(1) match { case s"  Starting items: $items" => items.split(", ").map(_.toLong).toList },
        operation = lines(2) match { case s"  Operation: new = $op" => parseOperation(op) },
        divisibleBy = lines(3) match { case s"  Test: divisible by $n" => n.toInt },
        trueMonkey = lines(4) match { case s"    If true: throw to monkey $n" => n.toInt },
        falseMonkey = lines(5) match { case s"    If false: throw to monkey $n" => n.toInt }
      )

  def getInput(file: String): Iterator[Monkey] =
    io.Source.fromResource(file)
      .mkString
      .split("\n\n").iterator
      .map(section => Monkey.fromString(section.split('\n')))

  def runMonkeys(monkeys: collection.mutable.IndexedSeq[Monkey], divisor: Int, rounds: Int): Long =
    val modulus = monkeys.map(_.divisibleBy).product
    for round <- 1 to rounds do
      for (m, i) <- monkeys.zipWithIndex do
        for item <- m.items do
          m.inspected += 1
          val newWorryLevel = m.operation(item)
          val reducedWorryLevel = (newWorryLevel / divisor) % modulus
          if reducedWorryLevel % m.divisibleBy == 0 then
            monkeys(m.trueMonkey).items :+= reducedWorryLevel
          else
            monkeys(m.falseMonkey).items :+= reducedWorryLevel
        m.items = Nil
    monkeys.map(_.inspected).sorted.takeRight(2).map(_.toLong).product

  // part 1 tests

  def testDay11Part1(name: String, file: String, expected: Long) =
    test(s"day 11 $name") {
      val input = getInput(file).to(collection.mutable.IndexedSeq)
      assertEquals(runMonkeys(input, divisor = 3, rounds = 20), expected)
    }

  testDay11Part1("part 1 sample", "day11-sample.txt", 10605)
  testDay11Part1("part 1",        "day11.txt",        78960)

  // part 2 tests

  def testDay11Part2(name: String, file: String, expected: Long) =
    test(s"day 11 $name") {
      val input = getInput(file).to(collection.mutable.IndexedSeq)
      assertEquals(runMonkeys(input, divisor = 1, rounds = 10000), expected)
    }

  testDay11Part2("part 2 sample", "day11-sample.txt", 2713310158L)
  testDay11Part2("part 2",        "day11.txt",        14561971968L)

end Day11
