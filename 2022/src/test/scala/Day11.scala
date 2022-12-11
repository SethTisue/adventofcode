class Day11 extends munit.FunSuite:

  case class Monkey(
    var items: List[Int],
    var inspected: Int = 0,
    operation: Int => Int,
    divisibleBy: Int,
    trueMonkey: Int,
    falseMonkey: Int)

  object Monkey:
    def parseOperation(s: String): Int => Int =
      s match
        case s"$left $middle $right" =>
          n =>
            val op1 = if left  == "old" then n else left.toInt
            val op2 = if right == "old" then n else right.toInt
            middle match
              case "+" => op1 + op2
              case "*" => op1 * op2
    def fromString(lines: Array[String]): Monkey =
      Monkey(
        items = lines(1) match { case s"  Starting items: $items" => items.split(", ").map(_.toInt).toList },
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

  def runMonkeys(monkeys: collection.mutable.IndexedSeq[Monkey]): Int =
    for round <- 1 to 20 do
      for (m, i) <- monkeys.zipWithIndex do
        // println(s"Monkey $i:")
        for item <- m.items do
          m.inspected += 1
          // println(s"  Monkey inspects an item with a worry level of $item")
          val newWorryLevel = m.operation(item)
          // println(s"    Worry level becomes $newWorryLevel.")
          val reducedWorryLevel = newWorryLevel / 3
          // println(s"    Monkey gets bored with item. Worry level is divided by 3 to $reducedWorryLevel")
          if reducedWorryLevel % m.divisibleBy == 0 then
            monkeys(m.trueMonkey).items :+= reducedWorryLevel
            // println(s"    Current worry level is divisible by ${m.divisibleBy}.")
            // println(s"    Item with worry level $reducedWorryLevel is thrown to monkey ${m.trueMonkey}.")
          else
            monkeys(m.falseMonkey).items :+= reducedWorryLevel
            // println(s"    Current worry level is not divisible by ${m.divisibleBy}.")
            // println(s"    Item with worry level $reducedWorryLevel is thrown to monkey ${m.falseMonkey}.")
        m.items = Nil
      println(s"After round $round, the monkeys are holding items with these worry levels:")
      for (m, i) <- monkeys.zipWithIndex do
        println(s"""Monkey $i: ${m.items.mkString(", ")}""")
      println()
    monkeys.map(_.inspected).sorted.takeRight(2).product

  // part 1 tests

  def testDay11Part1(name: String, file: String, expected: Int) =
    test(s"day 11 $name") {
      val input = getInput(file).to(collection.mutable.IndexedSeq)
      assertEquals(runMonkeys(input), expected)
    }

  testDay11Part1("part 1 sample", "day11-sample.txt", 10605)
  testDay11Part1("part 1",        "day11.txt",        78960)

end Day11
