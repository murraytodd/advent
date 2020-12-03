package advent.Y2019

import org.scalatest.{FunSuite, Matchers}
import advent.Y2019.Day2._

class TestDay2 extends FunSuite with Matchers {

  val program: State = advent.getDataWithTransform("data/2019-day2-input.txt")(_.split(",").map(_.toInt))
    .get.flatten.toVector

  test("Part 1 Samples") {
    compute(Vector(1,0,0,0,99)) shouldBe Vector(2,0,0,0,99)
    compute(Vector(2,3,0,3,99)) shouldBe Vector(2,3,0,6,99)
    compute(Vector(2,4,4,5,99,0)) shouldBe Vector(2,4,4,5,99,9801)
    compute(Vector(1,1,1,4,99,5,6,0,99)) shouldBe Vector(30,1,1,4,2,5,6,0,99)
  }

  test("Part 1") {
    val fixed = program.updated(1, 12).updated(2, 2)
    compute(fixed)(0) shouldBe 4576384
  }

  test("Part 2") {
    val inputs = for {
      noun <- 0 to 98
      verb <- 0 to 98
      fixed = program.updated(1,noun).updated(2,verb)
      if compute(fixed)(0) == 19690720
    } yield(noun * 100 + verb)
    inputs shouldBe Seq(5398)
  }
}
