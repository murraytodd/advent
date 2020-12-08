package advent.Y2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues

class TestDay8 extends AnyFunSuite with Matchers with EitherValues:

  import Day8._
  
  val sampleProgram: Program = """nop +0
                                 |acc +1
                                 |jmp +4
                                 |acc +3
                                 |jmp -3
                                 |acc -99
                                 |acc +1
                                 |jmp -4
                                 |acc +6""".stripMargin
    .split("\n").map(Instruction.apply).toIndexedSeq
  
  val testProgram = advent.getDataWithTransform("data/2020-day8-input.txt")(Instruction.apply)
    .get.toIndexedSeq
  
  test("Part 1 Sample") {
    sampleProgram.findRepeat() shouldBe Left(ComputeState(1,5))
  }
  
  test("Part 1") {
    testProgram.findRepeat().left.value.accum shouldBe 1867 
  }
  
  test("Part 2 Sample") {
    sampleProgram.fix shouldBe Some((7,8))
  }
  
  test("Part 2") {
    testProgram.fix shouldBe Some((277,1303))
  }