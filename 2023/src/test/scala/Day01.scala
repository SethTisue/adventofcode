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

  def calibrationValuePart2(line: String): Int =
    val words = Seq("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    val digits = line.tails.toList.init.flatMap: rest =>
      if rest.head.isDigit
      then Some(rest.head - '0')
      else
        val index = words.indexWhere(rest.startsWith)
        if index == -1
        then None
        else Some(index)
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
