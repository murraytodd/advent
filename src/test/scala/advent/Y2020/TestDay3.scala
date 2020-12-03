package advent.Y2020

import advent.Y2020.Day3.TreeMap
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.TryValues._

class TestDay3 extends FunSuite with Matchers {

  val sampleData = """..##.......
                   |#...#...#..
                   |.#....#..#.
                   |..#.#...#.#
                   |.#...##..#.
                   |..#.##.....
                   |.#.#.#....#
                   |.#........#
                   |#.##...#...
                   |#...##....#
                   |.#..#...#.#""".stripMargin.split("\n").toIndexedSeq

  val sampleTrees = new TreeMap(sampleData)

  val testData = advent.getData("data/2020-day3-input.txt")

  val testTrees = testData.map(d => new TreeMap(d))

  val slopes = Seq((1,1), (3,1), (5,1), (7,1), (1,2))

  test("Problem 1 Sample") {
    sampleTrees.countSlope(3,1) shouldBe 7
  }

  test("Problem 1") {
    testTrees.success.value.countSlope(3, 1) shouldBe 198
  }

  test("Problem 2 Sample") {
    slopes.map(s => sampleTrees.countSlope(s._1,s._2)).foldRight(1)(_ * _) shouldBe 336
  }

  test("Problem 2") {
    slopes.map(s => testTrees.success.value.countSlope(s._1,s._2)).foldRight(1L)(_ * _) shouldBe 5140884672L
  }
}
