package advent.Y2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TestDay7 extends AnyFunSuite with Matchers:
  import TestDay7._
  import Day7._

  test("Part 1 Example") {
    implicit val rules: Rules = sampleData.split("\n").map(Rule.apply).toMap
    Color("bright","white") canContain Color("shiny","gold") shouldBe true
    Color("muted","yellow") canContain Color("shiny","gold") shouldBe true
    Color("dark","orange") canContain Color("shiny","gold") shouldBe true
    Color("light","red") canContain Color("shiny","gold") shouldBe true
    rules.keySet.count(_ canContain Color("shiny","gold")) shouldBe 4
  }

  test("Part 1") {
    implicit val rules: Rules = testData.map(Rule.apply).toMap
    rules.keySet.count(_ canContain Color("shiny","gold")) shouldBe 261
  }

  test("Part 2 First Example") {
    implicit val rules: Rules = sampleData.split("\n").map(Rule.apply).toMap
    Color("shiny","gold").requires shouldBe 32
  }
  
  test("Part 2 Second Example") {
    implicit val rules: Rules = sampleData2.split("\n").map(Rule.apply).toMap
    Color("shiny","gold").requires shouldBe 126
  }
  
  test("Part 2") {
    implicit val rules: Rules = testData.map(Rule.apply).toMap
    Color("shiny","gold").requires shouldBe 3765
  }

object TestDay7:
  val sampleData = """light red bags contain 1 bright white bag, 2 muted yellow bags.
                     |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
                     |bright white bags contain 1 shiny gold bag.
                     |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
                     |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
                     |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
                     |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
                     |faded blue bags contain no other bags.
                     |dotted black bags contain no other bags.""".stripMargin

  val sampleData2 = """shiny gold bags contain 2 dark red bags.
                      |dark red bags contain 2 dark orange bags.
                      |dark orange bags contain 2 dark yellow bags.
                      |dark yellow bags contain 2 dark green bags.
                      |dark green bags contain 2 dark blue bags.
                      |dark blue bags contain 2 dark violet bags.
                      |dark violet bags contain no other bags.""".stripMargin
  
  val testData = advent.getData("data/2020-day7-input.txt").get
