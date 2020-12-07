package advent.Y2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TestDay6 extends AnyFunSuite with Matchers {
  import Day6._  

  val testData = advent.getGroups("data/2020-day6-input.txt").get

  val sampleData = """abc
                     |
                     |a
                     |b
                     |c
                     |
                     |ab
                     |ac
                     |
                     |a
                     |a
                     |a
                     |a
                     |
                     |b""".stripMargin

  test("Part 1 Example") {
    advent.toGrouped(sampleData).map(uniques).sum shouldBe 11
  }

  test("Part 1") {
    testData.map(uniques).sum shouldBe 7110
  }

  test("Part 2 Example") {
    advent.toGrouped(sampleData).map(allYes).sum shouldBe 6
  }

  test("Part 2") {
    testData.map(allYes).sum shouldBe 3628
  }
  
}
