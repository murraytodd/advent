package advent.Y2019

import scala.util.Using

object Day3 {

  val data = Using(scala.io.Source.fromFile("2019-day3-input.txt")) { source =>
    val lines = source.getLines()
    (lines.next(), lines.next())
  }.get

  case class Point(x: Int, y: Int) {
    def -(p: Point): Int = Math.abs(x - p.x) + Math.abs(y - p.y)

    /**
     * Return all points of the wire's path
     * @param instruction Direction followed by # of steps
     * @return List of positions, ending with the next juncture
     */
    def move(instruction: String): Seq[Point] = {
      val (direction: Char, n: Int) = (instruction.head, instruction.tail.toInt)
      direction match {
        case 'R' => (1 to n).map(i => Point(x + i, y))
        case 'L' => (1 to n).map(i => Point(x - i, y))
        case 'U' => (1 to n).map(i => Point(x, y + i))
        case 'D' => (1 to n).map(i => Point(x, y - i))
        case _ => throw new Exception("Illegal direction")
      }
    }
  }

  def runInstructions(start: Point, instructions: Seq[String]): Seq[Seq[Point]] = {
    if (instructions.isEmpty) {
      Nil
    } else {
      val steps = start.move(instructions.head)
      steps +: runInstructions(steps.last, instructions.tail)
    }
  }

  def closestDistance(inst1: String, inst2: String): Int = {
    val wire1 = runInstructions(Point(0,0), inst1.split(",").toIndexedSeq).flatten.toSet
    val wire2 = runInstructions(Point(0,0), inst2.split(",").toIndexedSeq).flatten.toSet
    (wire1 intersect wire2).map(_ - Point(0,0)).min
  }

  def shortestSignal(inst1: String, inst2: String): Int = {
    val wire1 = runInstructions(Point(0,0), inst1.split(",").toIndexedSeq).flatten
    val wire2 = runInstructions(Point(0,0), inst2.split(",").toIndexedSeq).flatten
    val intersections = wire1.toSet intersect wire2.toSet
    val lengths = for {
      i <- intersections
      length1 = wire1.indexWhere(_ == i) + 1
      length2 = wire2.indexWhere(_ == i) + 1
    } yield (length1 + length2)
    lengths.min
  }
}
