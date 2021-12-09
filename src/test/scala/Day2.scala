class Day2 extends munit.FunSuite:

  val sample = List("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")

  enum Command
    case Forward(n: Int)
    case Up(n: Int)
    case Down(n: Int)

  test("sample") {
    assertEquals(increases(sample), 7)
  }

end Day2
