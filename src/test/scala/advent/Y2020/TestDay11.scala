package advent.Y2020

import advent.Y2020.TestDay11.{part1Converged, part1State1, part1State2, part1State3, part1State4}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TestDay11 extends AnyFunSuite with Matchers {
  
  import Day11._
  import Day11.Space._
  import TestDay11._

  val p1s1 = Seating.fromString(part1State1)
  val p1s2 = Seating.fromString(part1State2)
  val p1s3 = Seating.fromString(part1State3)
  val p1s4 = Seating.fromString(part1State4)
  val p1converged = Seating.fromString(part1Converged)
  val data = Seating.fromString(testData)
  
  test("Part 1 Simple State changes") {
    p1s1.get(1,1) shouldBe Some(EmptySeat)
    p1s1.get(0,1) shouldBe Some(Floor)
    p1s2.get(1,1) shouldBe Some(FullSeat)
    p1s2.get(0,1) shouldBe Some(Floor)
    p1s1.neighbors(1,1) shouldBe 0
    p1s2.neighbors(1,1) shouldBe 6
    p1s2.neighbors(2,1) shouldBe 8
  }

  test("Part 1 State changes") {
    p1s1.updateState shouldBe p1s2
    p1s2.updateState shouldBe p1s3
    p1s1.toConvergence shouldBe p1converged
  }

  test("Part 1") {
    data.toConvergence.spaces.flatten.count(_ == Space.FullSeat) shouldBe 2261
  }

  test("Part 2 Examples") {
    val firstExample = Seating.fromString(part2Example1)
    firstExample.lineOfSightNeighbors(4,3) shouldBe 8

    val secondExample = Seating.fromString(part2Example2)
    secondExample.lineOfSightNeighbors(1,1) shouldBe 0
    
    val thirdExample = Seating.fromString(part2Example3)
    thirdExample.lineOfSightNeighbors(3,3) shouldBe 0

    val p2s3 = Seating.fromString(part2State3)
    val p2s4 = Seating.fromString(part2State4)
    val p2converged = Seating.fromString(part2LastState)

    p1s1.updateLineOfSightState shouldBe p1s2 // same as part one
    p1s2.updateLineOfSightState shouldBe p2s3
    p2s3.updateLineOfSightState shouldBe p2s4
    p1s1.lineOfSightToConvergence shouldBe p2converged
  }

  test("Part 2") {
    data.lineOfSightToConvergence.spaces.flatten.count(_ == Space.FullSeat) shouldBe 2039
  }

}

object TestDay11 {

  val part1State1 = """L.LL.LL.LL
                      |LLLLLLL.LL
                      |L.L.L..L..
                      |LLLL.LL.LL
                      |L.LL.LL.LL
                      |L.LLLLL.LL
                      |..L.L.....
                      |LLLLLLLLLL
                      |L.LLLLLL.L
                      |L.LLLLL.LL""".stripMargin.split("\n")
  
  val part1State2 = """#.##.##.##
                      |#######.##
                      |#.#.#..#..
                      |####.##.##
                      |#.##.##.##
                      |#.#####.##
                      |..#.#.....
                      |##########
                      |#.######.#
                      |#.#####.##""".stripMargin.split("\n")
  
  val part1State3 = """#.LL.L#.##
                      |#LLLLLL.L#
                      |L.L.L..L..
                      |#LLL.LL.L#
                      |#.LL.LL.LL
                      |#.LLLL#.##
                      |..L.L.....
                      |#LLLLLLLL#
                      |#.LLLLLL.L
                      |#.#LLLL.##""".stripMargin.split("\n")
  
  val part1State4 = """#.##.L#.##
                      |#L###LL.L#
                      |L.#.#..#..
                      |#L##.##.L#
                      |#.##.LL.LL
                      |#.###L#.##
                      |..#.#.....
                      |#L######L#
                      |#.LL###L.L
                      |#.#L###.##""".stripMargin.split("\n")
  
  val part1Converged = """#.#L.L#.##
                         |#LLL#LL.L#
                         |L.#.L..#..
                         |#L##.##.L#
                         |#.#L.LL.LL
                         |#.#L#L#.##
                         |..L.L.....
                         |#L#L##L#L#
                         |#.LLLLLL.L
                         |#.#L#L#.##""".stripMargin.split("\n")

  val part2Example1 = """.......#.
                        |...#.....
                        |.#.......
                        |.........
                        |..#L....#
                        |....#....
                        |.........
                        |#........
                        |...#.....""".stripMargin.split("\n")
  
  val part2Example2 = """.............
                        |.L.L.#.#.#.#.
                        |.............""".stripMargin.split("\n")
  
  val part2Example3 = """.##.##.
                        |#.#.#.#
                        |##...##
                        |...L...
                        |##...##
                        |#.#.#.#
                        |.##.##.""".stripMargin.split("\n")
  
  val part2State3 = """#.LL.LL.L#
                      |#LLLLLL.LL
                      |L.L.L..L..
                      |LLLL.LL.LL
                      |L.LL.LL.LL
                      |L.LLLLL.LL
                      |..L.L.....
                      |LLLLLLLLL#
                      |#.LLLLLL.L
                      |#.LLLLL.L#""".stripMargin.split("\n")
  
  val part2State4 = """#.L#.##.L#
                      |#L#####.LL
                      |L.#.#..#..
                      |##L#.##.##
                      |#.##.#L.##
                      |#.#####.#L
                      |..#.#.....
                      |LLL####LL#
                      |#.L#####.L
                      |#.L####.L#""".stripMargin.split("\n")
  
  val part2LastState = """#.L#.L#.L#
                         |#LLLLLL.LL
                         |L.L.L..#..
                         |##L#.#L.L#
                         |L.L#.LL.L#
                         |#.LLLL#.LL
                         |..#.L.....
                         |LLL###LLL#
                         |#.LLLLL#.L
                         |#.L#LL#.L#""".stripMargin.split("\n")
  
  val testData = advent.getData("data/2020-day11-input.txt").map(_.toArray).get

}