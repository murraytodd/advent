package advent.Y2019

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TestDay1 extends AnyFunSuite with Matchers:
  import Day1._

  val modules = advent.getDataWithTransform("data/2019-day1-input.txt")(_.toInt).get

  test("Part 1 Examples") {
    computeFuel(12) shouldBe 2
    computeFuel(14) shouldBe 2
    computeFuel(1969) shouldBe 654
    computeFuel(100756) shouldBe 33583
  }

  test("Part 1") {
    modules.map(computeFuel).sum shouldBe 3497998
  }

  test("Part 2 Examples") {
    aggregateFuel(14) shouldBe 2
    aggregateFuel(1969) shouldBe 966
    aggregateFuel(100756) shouldBe 50346
  }

  test("Part 2") {
    modules.map(aggregateFuel).sum shouldBe 5244112
  }
