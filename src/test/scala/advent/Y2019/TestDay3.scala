package advent.Y2019

import org.scalatest.{FunSuite, Matchers}

class TestDay3 extends FunSuite with Matchers {

  import Day3._

  val data = {
    val inputs = advent.getData("data/2019-day3-input.txt").get
    (inputs(0),inputs(1)) // just two lines
  }

  val sampleWires = Seq(
    ("R8,U5,L5,D3","U7,R6,D4,L4"),
    ("R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83"),
    ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
  )

  test("Part 1 Examples") {
    (closestDistance _).tupled(sampleWires(0)) shouldBe 6
    (closestDistance _).tupled(sampleWires(1)) shouldBe 159
    (closestDistance _).tupled(sampleWires(2)) shouldBe 135
  }

  test("Part 1") {
    (closestDistance _).tupled(data) shouldBe 1285
  }

  test("Part 2 Examples") {
    (shortestSignal _).tupled(sampleWires(0)) shouldBe 30
    (shortestSignal _).tupled(sampleWires(1)) shouldBe 610
    (shortestSignal _).tupled(sampleWires(2)) shouldBe 410
  }

  test("Part 2") {
    (shortestSignal _).tupled(data) shouldBe 14228
  }
}
