class Day01 extends munit.FunSuite:

  def getInput(name: String): List[String] =
    io.Source.fromResource(name)
      .getLines.toList

// part 1

  def calibrationValuePart1(line: String): Int =
    val digits = line.filter(_.isDigit)
    s"${digits.head}${digits.last}".toInt

  test("day 1 part 1 sample") {
    assertEquals(142,
      getInput("day01-sample1.txt")
        .map(calibrationValuePart1).sum)
  }

  test("day 1 part 1") {
    assertEquals(56042,
      getInput("day01.txt")
        .map(calibrationValuePart1).sum)
  }

  // part 2

  val words = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  def calibrationValuePart2(line: String): Int =
    val digits = line.tails.toList.filter(_.nonEmpty).flatMap: rest =>
      if rest.head.isDigit
      then Some(rest.head.toString.toInt)
      else
        Some(words.indexWhere(rest.startsWith))
          .filter(_ >= 0)
          .map(_ + 1)
    digits.head * 10 + digits.last

  test("day 1 part 2 sample") {
    assertEquals(281,
      getInput("day01-sample2.txt")
        .map(calibrationValuePart2).sum)
  }

  test("day 1 part 2") {
    assertEquals(55358,
      getInput("day01.txt")
        .map(calibrationValuePart2).sum)
  }

end Day01
