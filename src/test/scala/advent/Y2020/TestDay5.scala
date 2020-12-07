package advent.Y2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TestDay5 extends AnyFunSuite with Matchers {
  import Day5._  

  val data = advent.getDataWithTransform("data/2020-day5-input.txt")(Seat.apply).get

  test("Part 1 Examples") {
    Seat("BFFFBBFRRR").id shouldBe 567
    Seat("FFFBBBFRRR").id shouldBe 119
    Seat("BBFFBBFRLL").id shouldBe 820
  }

  test("Part 1") {
    data.map(_.id).max shouldBe 963
  }

  test("Part 2") {
    val ids = data.map(_.id).toSet
    val range = (ids.min to ids.max).toSet
    val missing = (range diff ids)
    missing.size shouldBe 1
    missing.head shouldBe 592
  }

}
