package advent.Y2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TestDay10 extends AnyFunSuite with Matchers {
  
  import Day10._
  import TestDay10._

  test("Part 1 Example") {
    adapterSteps(simpleSample) shouldBe Map(1 -> 7, 3 -> 5)
    adapterSteps(longerSample) shouldBe Map(1 -> 22, 3 -> 10)
  }

  test("Part 1") {
    adapterSteps(testJolts) shouldBe Map(1 -> 69, 3 -> 40)
  }

  test("Part 2 Example") {
    adapterPermutations(simpleSample) shouldBe 8L
    adapterPermutations(longerSample) shouldBe 19208L
    adapterPermutations(testJolts) shouldBe 13816758796288L
  }
}

object TestDay10 {

  val simpleSample = List(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)

  val longerSample = List(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 
    49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)

  val testJolts = advent.getDataWithTransform("data/2020-day10-input.txt")(_.toInt).get.toList

}
