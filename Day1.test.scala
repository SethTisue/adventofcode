// using scala 3.1.1-RC1
// using lib org.scalameta::munit::0.7.29

class Day1 extends munit.FunSuite:

  def increases(xs: List[Int]): Int =
    xs.sliding(2).count {
      case List(prev, cur) =>
        cur > prev
    }

  test("sample") {
    val depths = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
    assertEquals(increases(depths), 7)
  }

  test("real") {
    val depths = io.Source.fromFile("day1.txt").getLines.flatMap(_.toIntOption).toList
    assertEquals(increases(depths), 1713)
  }
