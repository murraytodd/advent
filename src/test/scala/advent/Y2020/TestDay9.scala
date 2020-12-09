package advent.Y2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TestDay9 extends AnyFunSuite with Matchers {
  
  import Day9._

  val sampleData = Seq(35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576)
    .map(_.toLong)
  val testData = advent.getDataWithTransform("data/2020-day9-input.txt")(_.toLong).get

  test("Part 1 Example") {
    val sampleSeq = (1L to 25L).toSeq
    validate(sampleSeq, 26) shouldBe true
    validate(sampleSeq, 49) shouldBe true
    validate(sampleSeq, 100) shouldBe false
    validate(sampleSeq, 50) shouldBe false

    firstInvalid(sampleData,5) shouldBe Some(127)
  }

  test("Part 1") {
    firstInvalid(testData,25) shouldBe Some(258585477L)
  }

  test("Part 2 Example") {
    findContiguousRange(sampleData,127) shouldBe Some(15L,47L)
    findContiguousRange(testData,258585477L) shouldBe Some(9455395L, 27525818L)
  }
  
}
