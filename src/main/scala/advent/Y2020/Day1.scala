package advent.Y2020

object Day1 {

  def findPairMulti(target: Int, data: Seq[Int]): Long = {
    val answer = for {
      x <- data
      y <- data if (x < y) && (x + y == target)
    } yield (x * y)
    assert(answer.size == 1, "There should be one unique result.")
    answer.head
  }

  def findTrioMulti(target: Int, data: Seq[Int]): Long = {
    val answer = for {
      x <- data
      y <- data if (x < y)
      z <- data if (y < z) && (x + y + z == target)
    } yield (x.toLong * y * z)
    assert(answer.size == 1, "There should be one unique result")
    answer.head
  }
}
