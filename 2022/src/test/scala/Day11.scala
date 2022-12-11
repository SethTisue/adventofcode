// This was so easy/natural to model with mutability --
// I don't feel like refactoring it to be purely functional.

class Day11 extends munit.FunSuite:

  // this much is immutable

  case class Behavior(
    operation: Long => Long,
    divisor: Int,
    trueMonkey: Int,
    falseMonkey: Int
  )
  object Behavior:
    def parseOperation(s: String): Long => Long =
      s match
        case s"$left $middle $right" =>
          n =>
            val arg1 = if left  == "old" then n else left.toLong
            val arg2 = if right == "old" then n else right.toLong
            middle match
              case "+" => arg1 + arg2
              case "*" => arg1 * arg2

  // this is mutable

  case class Monkey(
    behavior: Behavior,
    var items: List[Long],
    var inspected: Int = 0
  )
  object Monkey:
    def fromString(lines: Array[String]): Monkey =
      Monkey(
        items = lines(1) match { case s"  Starting items: $items" => items.split(", ").map(_.toLong).toList },
        behavior = Behavior(
          operation = lines(2) match { case s"  Operation: new = $op" => Behavior.parseOperation(op) },
          divisor = lines(3) match { case s"  Test: divisible by $n" => n.toInt },
          trueMonkey = lines(4) match { case s"    If true: throw to monkey $n" => n.toInt },
          falseMonkey = lines(5) match { case s"    If false: throw to monkey $n" => n.toInt }
        ))

  def runMonkeys(monkeys: IndexedSeq[Monkey], worryReduction: Int, rounds: Int): Long =
    val modulus = monkeys.map(_.behavior.divisor).product
    for round <- 1 to rounds
        (m, i) <- monkeys.zipWithIndex do
      for item <- m.items do
        m.inspected += 1
        val reducedWorryLevel = (m.behavior.operation(item) / worryReduction) % modulus
        val nextMonkey =
          if reducedWorryLevel % m.behavior.divisor == 0
          then m.behavior.trueMonkey
          else m.behavior.falseMonkey
        monkeys(nextMonkey).items :+= reducedWorryLevel
      m.items = Nil
    monkeys.map(_.inspected).sorted.takeRight(2)
      .map(_.toLong).product

  // testing

  def getInput(file: String): IndexedSeq[Monkey] =
    io.Source.fromResource(file)
      .mkString
      .split("\n\n")
      .map(section => Monkey.fromString(section.split('\n')))
      .to(IndexedSeq)

  def testDay11(name: String, file: String, worryReduction: Int, rounds: Int, expected: Long) =
    test(s"day 11 $name") {
      assertEquals(runMonkeys(getInput(file), worryReduction, rounds), expected)
    }

  testDay11("part 1 sample", "day11-sample.txt", 3, 20, 10605)
  testDay11("part 1",        "day11.txt",        3, 20, 78960)

  testDay11("part 2 sample", "day11-sample.txt", 1, 10000, 2713310158L)
  testDay11("part 2",        "day11.txt",        1, 10000, 14561971968L)

end Day11
