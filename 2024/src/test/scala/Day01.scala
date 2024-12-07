class Day01 extends munit.FunSuite:

  def getInput(name: String): (List[Int], List[Int]) =
    val pairs =
      io.Source.fromResource(name)
        .getLines
        .map: line =>
          line match
            case s"$item1   $item2" =>
              (item1.toInt, item2.toInt)
        .toList
    (pairs.map(_._1).toList, pairs.map(_._2).toList)

  // part 1

  test("part 1 sample"):
    val (input1, input2) = getInput("day01-sample.txt")
    assertEquals(totalDistance(input1, input2),
      11)

  test("part 1"):
    val (input1, input2) = getInput("day01.txt")
    assertEquals(1320851,
      totalDistance(input1, input2))

  def totalDistance(l1: List[Int], l2: List[Int]): Int =
    l1.sorted.zip(l2.sorted).map: (n1, n2) =>
        math.abs(n1 - n2)
      .sum

  // part 2

  test("part 2 sample"):
    val (input1, input2) = getInput("day01-sample.txt")
    assertEquals(similarity(input1, input2),
      31)

  test("part 2"):
    val (input1, input2) = getInput("day01.txt")
    assertEquals(26859182,
      similarity(input1, input2))

  def similarity(l1: List[Int], l2: List[Int]): Int =
    l1.map: n1 =>
      n1 * l2.count(_ == n1)
    .sum

end Day01
