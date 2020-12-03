package advent.Y2020

import advent.Y2020.Day2.Entry
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.TryValues._

class TestDay2 extends FunSuite with Matchers {

  val data = advent.getDataWithTransform("data/2020-day2-input.txt")(Entry.apply)

  test("Instructions Part 1") {
    Day2.Entry("1-3 a: abcde").isValid shouldBe true
    Day2.Entry("1-3 b: cdefg").isValid shouldBe false
    Day2.Entry("2-9 c: ccccccccc").isValid shouldBe true
  }

  test("Instructions Part 2") {
    Day2.Entry("1-3 a: abcde").isValid2 shouldBe true
    Day2.Entry("1-3 b: cdefg").isValid2 shouldBe false
    Day2.Entry("2-9 c: ccccccccc").isValid2 shouldBe false
  }

  test("Part 1") {
    data.success.value.count(_.isValid) shouldBe 416
  }

  test("Part 2") {
    data.success.value.count(_.isValid2) shouldBe 688
  }

}
