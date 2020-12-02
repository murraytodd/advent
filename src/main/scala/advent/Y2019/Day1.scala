package advent.Y2019

import scala.util.Using

object Day1 {

  val modules = Using(scala.io.Source.fromFile("day1-input.txt")) { source =>
    source.getLines().map(_.toInt).toList
  }.get

  def computeFuel(weight: Int): Int = Math.max(0, weight / 3 - 2)

  def aggregateFuel(weight: Int): Int = {
    val fuel = computeFuel(weight)
    if (fuel == 0)
      0
    else
      fuel + aggregateFuel(fuel)
  }

  def puzzle1 = modules.map(computeFuel).sum

  def puzzle2 = modules.map(aggregateFuel).sum

}
