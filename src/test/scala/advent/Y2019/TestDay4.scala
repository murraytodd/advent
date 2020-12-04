package advent.Y2019

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TestDay4 extends AnyFunSuite with Matchers:

  import Day4._

  test("Part 1 Examples") {
    Password("111111").isValid shouldBe true
    Password("223450").isValid shouldBe false
    Password("123789").isValid shouldBe false
  }

  test("Part 1 test range") {
    (254032 to 789860).count(Password(_).isValid) shouldBe 1033
  }

  test("Part 2 Examples") {
    Password("112233").isValid2 shouldBe true
    Password("123444").isValid2 shouldBe false
    Password("111122").isValid2 shouldBe true
  }

  test("Part 2 test range") {
    (254032 to 789860).count(Password(_).isValid2) shouldBe 670
  }
