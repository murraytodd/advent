package advent.Y2019

object Day1 {

  def computeFuel(weight: Int): Int = Math.max(0, weight / 3 - 2)

  def aggregateFuel(weight: Int): Int = {
    val fuel = computeFuel(weight)
    if (fuel == 0)
      0
    else
      fuel + aggregateFuel(fuel)
  }

}
