package advent.Y2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


class TestDay1 extends AnyFunSuite with Matchers:

  val sampleData = Seq(1721, 979, 366, 299, 675, 1456)
  val testData = advent.getDataWithTransform("data/2020-day1-input.txt")(_.toInt).get

  test("Part 1 Example") {
    Day1.findPairMulti(2020, sampleData) shouldBe 514579
  }

  test("Part 1") {
    Day1.findPairMulti(2020, testData) shouldBe 838624
  }

  test("Part 2 Example") {
    Day1.findTrioMulti(2020, sampleData) shouldBe 241861950
  }

  test("Part 2") {
    Day1.findTrioMulti(2020, testData) shouldBe 52764180
  }
